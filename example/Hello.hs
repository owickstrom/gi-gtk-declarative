{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.Text          (Text)
import qualified GI.Gdk             as Gdk
import qualified GI.GLib.Constants  as GLib
import           GI.Gtk.Declarative hiding (main)
import qualified GI.Gtk.Declarative as Gtk

-- A very simple declarative user interface.
helloView :: (Text, Bool) -> Object
helloView (who, flipped) = container Box [] $
  op
  [ BoxChild True True 0 (node Label [#label := "This is a sample application."])
  , BoxChild True True 0 (node Label [#label := who])
  ]
  where
    op = if flipped then reverse else id

runUI :: IO () -> IO ()
runUI f = void (Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT (f *> return False))

mainLoop :: Gtk.Window -> Chan a -> (a -> Object) -> IO ()
mainLoop window models view = do
  first <- view <$> readChan models
  runUI $ Gtk.containerAdd window =<< Gtk.toWidget =<< create first
  loop first
  where
    loop old = do
      next <- view <$> readChan models
      runUI $ patchContainer window old next
      loop next
    patchContainer :: Gtk.Window -> Object -> Object -> IO ()
    patchContainer w o1 o2 =
      case patch o1 o2 of
        Modify f ->
          Gtk.containerGetChildren w >>= \case
            [] -> return ()
            (c:_) -> do
              f =<< Gtk.toWidget c
              Gtk.widgetShowAll w
        Replace createNew -> do
          Gtk.containerForall w (Gtk.containerRemove w)
          newWidget <- createNew
          Gtk.containerAdd w newWidget
          Gtk.widgetShowAll w
        Keep -> return ()

main :: IO ()
main = do
  -- Basic setup.
  void $ Gtk.init Nothing
  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  void (Gtk.onWidgetDestroy window mainQuit)

  -- Customize the top window.
  Gtk.windowSetTitle window "Sample gi-gtk-declarative app!"
  Gtk.windowResize window 640 480

  -- A channel of "model" values.
  models <- newChan

  -- We start a thread that sends new model values every second, with
  -- alternating True/False values.
  void . forkIO $
    mapM_
    (\(n, f) -> threadDelay 1000000 *> writeChan models ("Hello, " <> n, f))
    (cycle [("Joe", True), ("Mike", False)])

  -- And a thread for the main loop that listens for models, diffs the
  -- GUI, and re-renders the underlying GTK+ widgets when needed.
  void . forkIO $
    mainLoop window models helloView

  -- Let's do it!
  Gtk.main
