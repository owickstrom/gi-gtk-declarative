module GI.Gtk.Declarative.TestUtils where

import           Control.Concurrent
import           Control.Monad.IO.Class
import qualified GI.GLib.Constants             as GLib
import qualified GI.Gdk                        as Gdk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.State

runUI :: MonadIO m => IO b -> m b
runUI ma = do
  ret <- liftIO newEmptyMVar
  _   <- Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
    ma >>= putMVar ret
    return False
  liftIO (takeMVar ret)

patch'
  :: Patchable widget => SomeState -> widget e1 -> widget e2 -> IO SomeState
patch' state markup1 markup2 = case patch state markup1 markup2 of
  Keep      -> pure state
  Modify  f -> f
  Replace f -> f
