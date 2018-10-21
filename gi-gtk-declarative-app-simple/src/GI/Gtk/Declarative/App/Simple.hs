{-# LANGUAGE RecordWildCards #-}

-- | A simple application architecture style inspired by PureScript's Pux
-- framework.
module GI.Gtk.Declarative.App.Simple
  ( App(..)
  , AppView
  , Transition(..)
  , run
  )
where

import           Control.Concurrent
import           Control.Monad
import           Data.Typeable
import qualified GI.Gdk                        as Gdk
import qualified GI.GLib.Constants             as GLib
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.State
import           Pipes
import           Pipes.Concurrent

-- | Describes an state reducer application.
data App state event =
  App
    { update :: state -> event -> Transition state event
    -- ^ The update function of an application reduces the current state and
    -- a new event to a 'Transition', which decides if and how to transition
    -- to the next state.
    , view   :: state -> AppView event
    -- ^ The view renders a state value as a window, parameterized by the
    -- 'App's event type.
    , inputs :: [Producer event IO ()]
    -- ^ Inputs are pipes 'Producer's that feed events into the application.
    , initialState :: state
    -- ^ The initial state value of the state reduction loop.
    }

type AppView event = Bin Gtk.Window Widget event

-- | The result of applying the 'update' function, deciding if and how to
-- transition to the next state.
data Transition state event =
  -- Transition to the given state, and with an IO action that may return a
  -- new event.
  Transition state (IO (Maybe event))
  -- | Exit the application.
  | Exit

-- | Run an 'App'. This IO action will loop, so run it in a separate thread
-- using 'forkIO' if you're calling it before the GTK main loop.
runLoop :: Typeable event => App state event -> IO ()
runLoop App {..} = do
  let firstMarkup = view initialState
  nextEvent                  <- newEmptyMVar
  (firstState, subscription) <- do
    firstState <- runUI (create firstMarkup)
    let widget' = stateTreeNodeWidget firstState
    runUI (Gtk.widgetShowAll widget')
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
          case patch oldState oldMarkup newMarkup of
            Modify ma -> do
              runUI (cancel oldSubscription)
              newState <- runUI ma
              sub <- subscribe newMarkup (stateTreeNodeWidget newState) (publishEvent nextEvent)
              return (newState, sub)
            Replace createNew -> runUI $ do
              Gtk.widgetDestroy (stateTreeNodeWidget oldState)
              runUI (cancel oldSubscription)
              newState <- createNew
              Gtk.widgetShowAll (stateTreeNodeWidget newState)
              sub <- subscribe newMarkup (stateTreeNodeWidget newState) (publishEvent nextEvent)
              return (newState, sub)
            Keep -> return (oldState, oldSubscription)

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
  => App state event      -- ^ Application to run
  -> IO ()
run app = do
  void $ Gtk.init Nothing
  void . forkIO $ do
    runLoop app
    -- In case the run loop exits, quit the main GTK loop.
    Gtk.mainQuit
  Gtk.main

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
runUI ma = do
  r <- newEmptyMVar
  runUI_ (ma >>= putMVar r)
  takeMVar r

runUI_ :: IO () -> IO ()
runUI_ ma =
  void . Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
    ma
    return False
