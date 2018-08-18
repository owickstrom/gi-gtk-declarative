{-# LANGUAGE LambdaCase #-}
module MainLoop where

import           Control.Concurrent
import           Control.Monad
import qualified GI.Gdk             as Gdk
import qualified GI.GLib.Constants  as GLib
import           GI.Gtk.Declarative hiding (main)
import qualified GI.Gtk.Declarative as Gtk

runUI :: IO () -> IO ()
runUI f = void (Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT (f *> return False))

-- Our generic main loop, reading models from the Chan, applying the
-- view function, and patching the GTK+ widgets.
--
-- TODO: Extract this to the library (if would be a common useful pattern?)
mainLoop :: Gtk.Window -> Chan model -> (model -> Markup event) -> IO ()
mainLoop window models view = do
  first <- view <$> readChan models
  runUI $ do
    Gtk.containerAdd window =<< Gtk.toWidget =<< create first
    Gtk.widgetShowAll window
  loop first
  where
    loop old = do
      next <- view <$> readChan models
      runUI $ patchContainer window old next
      loop next
    patchContainer :: Gtk.Window -> Markup event -> Markup event -> IO ()
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
