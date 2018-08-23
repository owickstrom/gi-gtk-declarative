{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module GI.Gtk.Declarative.App.Simple
  ( App(..)
  , runInWindow
  , run
  )
where

import           Data.Int
import           Data.Text                                ( Text )
import           Data.Typeable
import           Control.Concurrent
import           Control.Monad
import           Pipes
import           Pipes.Concurrent
import qualified GI.Gdk                        as Gdk
import qualified GI.GLib.Constants             as GLib
import           GI.Gtk.Declarative                hiding ( main )
import qualified GI.Gtk.Declarative            as Gtk
import           GI.Gtk.Declarative.EventSource

data App model event =
  App
    { update :: model -> event -> (model, IO (Maybe event))
    , view :: model -> Markup event
    , inputs :: [Producer event IO ()]
    }

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

runInWindow :: Typeable event => Gtk.Window -> App model event -> model -> IO ()
runInWindow window App {..} initialModel = do
  let firstMarkup = view initialModel
  nextEvent    <- newEmptyMVar
  subscription <- runUI $ do
    widget <- Gtk.toWidget =<< create firstMarkup
    Gtk.containerAdd window widget
    Gtk.widgetShowAll window
    subscribe firstMarkup widget (publishEvent nextEvent)
  void . forkIO $
    runEffect (mergeProducers inputs >-> publishInputEvents nextEvent)
  loop firstMarkup nextEvent subscription initialModel
 where
  loop oldMarkup nextEvent oldSubscription oldModel = do
    event <- takeMVar nextEvent
    let (newModel, action) = update oldModel event
        newMarkup          = view newModel

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

run
  :: Typeable event
  => Text
  -> Maybe (Int32, Int32)
  -> App model event
  -> model
  -> IO ()
run title size app initialModel = do
  void $ Gtk.init Nothing
  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  void (Gtk.onWidgetDestroy window mainQuit)
  Gtk.windowSetTitle window title
  case size of
    Just (width, height) -> Gtk.windowResize window width height
    Nothing              -> return ()
  void . forkIO $ runInWindow window app initialModel
  Gtk.main

patchContainer
  :: Typeable event
  => Gtk.Window
  -> Markup event
  -> Markup event
  -> MVar event
  -> IO (Maybe Subscription)
patchContainer w o1 o2 nextEvent = case patch o1 o2 of
  Modify f -> Gtk.containerGetChildren w >>= \case
    []      -> return Nothing
    (c : _) -> do
      widget <- Gtk.toWidget c
      f widget
      Gtk.widgetShowAll w
      Just <$> subscribe o2 widget (publishEvent nextEvent)
  Replace createNew -> do
    Gtk.containerForall w (Gtk.containerRemove w)
    newWidget <- createNew
    Gtk.containerAdd w newWidget
    Gtk.widgetShowAll w
    Just <$> subscribe o2 newWidget (publishEvent nextEvent)
  Keep -> return Nothing

runUI :: IO a -> IO a
runUI f = do
  r <- newEmptyMVar
  void . Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
    f >>= putMVar r
    return False
  takeMVar r
