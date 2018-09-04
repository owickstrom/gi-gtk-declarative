{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-- | A simple application architecture style inspired by PureScript's Pux
-- framework.
module GI.Gtk.Declarative.App.Simple
  ( App(..)
  , Continuation(..)
  , runInWindow
  , run
  )
where

import           Control.Concurrent
import           Control.Monad
import           Data.Int
import           Data.Text                      (Text)
import           Data.Typeable
import qualified GI.Gdk                         as Gdk
import qualified GI.GLib.Constants              as GLib
import qualified GI.Gtk                         as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.EventSource
import           Pipes
import           Pipes.Concurrent

-- | Describes an state reducer application.
data App state event =
  App
    { update :: state -> event -> Continuation state event
    -- ^ The update function of an application reduces the current state and
    -- a new event to a 'Continutation', which decides if and how to continue
    -- the loop.
    , view   :: state -> Widget event
    -- ^ The view renders a state value as a 'Widget', parameterized by the
    -- 'App's event type.
    , inputs :: [Producer event IO ()]
    -- ^ Inputs are pipes 'Producer's that feed events into the application.
    , initialState :: state
    -- ^ The initial state value of the state reduction loop.
    }

-- | The result of applying the 'update' function, deciding how to continue.
data Continuation state event =
  -- Continue with the given state, and with an IO action that may return a
  -- new event.
  Continue state (IO (Maybe event))
  -- | Exit the application.
  | Exit

-- | Run an 'App' in a 'Gtk.Window' that has already been set up. This IO action
-- will loop, so run it in a separate thread using 'forkIO' if you're calling
-- it before the GTK main loop.
runInWindow :: Typeable event => Gtk.Window -> App state event -> IO ()
runInWindow window App {..} = do
  let firstMarkup = view initialState
  nextEvent    <- newEmptyMVar
  subscription <- runUI $ do
    widget' <- Gtk.toWidget =<< create firstMarkup
    Gtk.containerAdd window widget'
    Gtk.widgetShowAll window
    subscribe firstMarkup widget' (publishEvent nextEvent)
  void . forkIO $
    runEffect (mergeProducers inputs >-> publishInputEvents nextEvent)
  loop firstMarkup nextEvent subscription initialState
 where
  loop oldMarkup nextEvent oldSubscription oldModel = do
    event <- takeMVar nextEvent
    case update oldModel event of
      Continue newModel action -> do
        let newMarkup          = view newModel

        sub <- runUI (patchContainer window oldMarkup newMarkup nextEvent) >>= \case
          Just newSubscription -> cancel oldSubscription *> pure newSubscription
          Nothing              -> pure oldSubscription

        -- Make sure the MVar is empty.
        void (tryTakeMVar nextEvent)

        -- If the action returned by the update function produced an event, then
        -- we write that as the nextEvent to use directly.
        void . forkIO $ action >>= maybe (return ()) (putMVar nextEvent)

        -- Finally, we loop.
        loop newMarkup nextEvent sub newModel
      Exit ->
        return ()

-- | Initialize GTK, set up a new window, and run the application in it. This
-- is a convenience function. If you need more flexibility, you should use
-- 'runInWindow' instead.
run
  :: Typeable event
  => Text
  -> Maybe (Int32, Int32)
  -> App state event
  -> IO ()
run title size app = do
  void $ Gtk.init Nothing
  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  void (Gtk.onWidgetDestroy window Gtk.mainQuit)
  Gtk.windowSetTitle window title
  case size of
    Just (width, height) -> Gtk.windowResize window width height
    Nothing              -> return ()
  void . forkIO $ do
    runInWindow window app
    -- In case the run loop exits, quit the main GTK loop.
    Gtk.mainQuit
  Gtk.main

patchContainer
  :: Typeable event
  => Gtk.Window
  -> Widget event
  -> Widget event
  -> MVar event
  -> IO (Maybe Subscription)
patchContainer w o1 o2 nextEvent = case patch o1 o2 of
  Modify f -> Gtk.containerGetChildren w >>= \case
    []      -> return Nothing
    (c : _) -> do
      widget' <- Gtk.toWidget c
      f widget'
      Gtk.widgetShowAll w
      Just <$> subscribe o2 widget' (publishEvent nextEvent)
  Replace createNew -> do
    Gtk.containerForall w (Gtk.containerRemove w)
    newWidget <- createNew
    Gtk.containerAdd w newWidget
    Gtk.widgetShowAll w
    Just <$> subscribe o2 newWidget (publishEvent nextEvent)
  Keep -> return Nothing

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
publishInputEvents nextEvent =
  forever (await >>= liftIO . (putMVar nextEvent))

runUI :: IO a -> IO a
runUI f = do
  r <- newEmptyMVar
  void . Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
    f >>= putMVar r
    return False
  takeMVar r
