{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-- | A simple application architecture style inspired by PureScript's Pux
-- framework.
module GI.Gtk.Declarative.App.Simple
  ( App(..)
  , Transition(..)
  , runInWindow
  , run
  )
where

import           Control.Concurrent
import           Control.Monad
import           Data.Int
import           Data.Text                      ( Text )
import           Data.Typeable
import qualified GI.Gdk                        as Gdk
import qualified GI.GLib.Constants             as GLib
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.EventSource
import           Pipes
import           Pipes.Concurrent

-- | Describes an state reducer application.
data App state event =
  App
    { update :: state -> event -> Transition state event
    -- ^ The update function of an application reduces the current state and
    -- a new event to a 'Transition', which decides if and how to transition
    -- to the next state.
    , view   :: state -> Widget event
    -- ^ The view renders a state value as a 'Widget', parameterized by the
    -- 'App's event type.
    , inputs :: [Producer event IO ()]
    -- ^ Inputs are pipes 'Producer's that feed events into the application.
    , initialState :: state
    -- ^ The initial state value of the state reduction loop.
    }

-- | The result of applying the 'update' function, deciding if and how to
-- transition to the next state.
data Transition state event =
  -- Transition to the given state, and with an IO action that may return a
  -- new event.
  Transition state (IO (Maybe event))
  -- | Exit the application.
  | Exit

-- | Run an 'App' in a 'Gtk.Window' that has already been set up. This IO action
-- will loop, so run it in a separate thread using 'forkIO' if you're calling
-- it before the GTK main loop.
runInWindow :: Typeable event => Gtk.Window -> App state event -> IO ()
runInWindow window App {..} = do
  let firstMarkup = view initialState
  nextEvent                  <- newEmptyMVar
  (firstState, subscription) <- runUI $ do
    firstState <- create firstMarkup
    widget'    <- Gtk.toWidget (shadowStateTopWidget firstState)
    Gtk.containerAdd window widget'
    Gtk.widgetShowAll window
    sub <- subscribe firstMarkup widget' (publishEvent nextEvent)
    return (firstState, sub)
  void . forkIO $ runEffect
    (mergeProducers inputs >-> publishInputEvents nextEvent)
  loop firstState firstMarkup nextEvent subscription initialState
 where
  loop oldState oldMarkup nextEvent oldSubscription oldModel = do
    event <- takeMVar nextEvent
    case update oldModel event of
      Transition newModel action -> do
        let newMarkup = view newModel

        (newState, sub) <-
          runUI (patchContainer window oldState oldMarkup newMarkup nextEvent)
            >>= \case
                  (newState, Just newSubscription) -> do
                    cancel oldSubscription
                    pure (newState, newSubscription)
                  (newState, Nothing) ->
                    pure (newState, oldSubscription)

        -- Make sure the MVar is empty.
        void (tryTakeMVar nextEvent)

        -- If the action returned by the update function produced an event, then
        -- we write that as the nextEvent to use directly.
        void . forkIO $ action >>= maybe (return ()) (putMVar nextEvent)

        -- Finally, we loop.
        loop newState newMarkup nextEvent sub newModel
      Exit -> return ()

-- | Initialize GTK, set up a new window, and run the application in it. This
-- is a convenience function. If you need more flexibility, you should use
-- 'runInWindow' instead.
run
  :: Typeable event
  => Text                 -- ^ Window title
  -> Maybe (Int32, Int32) -- ^ Optional window size
  -> App state event      -- ^ Application to run
  -> IO ()
run title size app = do
  void $ Gtk.init Nothing
  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  void (Gtk.onWidgetDestroy window Gtk.mainQuit)
  Gtk.windowSetTitle window title
  case size of
    Just (width, height) -> Gtk.windowResize window width height
    Nothing              -> return ()
  void
    . forkIO
    $ do
        runInWindow window app
        -- In case the run loop exits, quit the main GTK loop.
        Gtk.mainQuit
  Gtk.main

patchContainer
  :: Typeable event
  => Gtk.Window
  -> ShadowState
  -> Widget event
  -> Widget event
  -> MVar event
  -> IO (ShadowState, Maybe Subscription)
patchContainer w state o1 o2 nextEvent = case patch state o1 o2 of
  Modify ma -> do
    newState <- ma
    -- Gtk.widgetShowAll w
    sub <- subscribe o2 (shadowStateTopWidget newState) (publishEvent nextEvent)
    return (newState, Just sub)
  Replace createNew -> do
    Gtk.containerForall w (Gtk.containerRemove w)
    newState <- createNew
    Gtk.containerAdd w (shadowStateTopWidget newState)
    Gtk.widgetShowAll w
    sub <- subscribe o2 (shadowStateTopWidget newState) (publishEvent nextEvent)
    return (newState, Just sub)
  Keep -> return (state, Nothing)

publishEvent :: MVar event -> event -> IO ()
publishEvent mvar = void . tryPutMVar mvar

mergeProducers :: [Producer a IO ()] -> Producer a IO ()
mergeProducers producers = do
  (output, input) <- liftIO $ spawn unbounded
  _               <- liftIO $ mapM (fork output) producers
  fromInput input
 where
  fork :: Output a -> Producer a IO () -> IO ()
  fork output producer = void $ forkIO $ do
    runEffect $ producer >-> toOutput output
    performGC

publishInputEvents :: MVar event -> Consumer event IO ()
publishInputEvents nextEvent = forever (await >>= liftIO . (putMVar nextEvent))

runUI :: IO a -> IO a
runUI f = do
  r <- newEmptyMVar
  void . Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
    f >>= putMVar r
    return False
  takeMVar r
