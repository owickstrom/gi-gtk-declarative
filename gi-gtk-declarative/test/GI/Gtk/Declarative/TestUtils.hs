module GI.Gtk.Declarative.TestUtils where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception.Safe
import Control.Monad (replicateM_)
import Control.Monad.IO.Class
import Data.Function ((&))
import qualified Data.HashSet as HashSet
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified GI.GLib.Constants as GLib
import qualified GI.GObject as GI
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative
import GI.Gtk.Declarative.EventSource
import GI.Gtk.Declarative.State
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

runUI :: MonadIO m => IO b -> m b
runUI ma = do
  ret <- liftIO newEmptyMVar
  _ <- Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
    ma >>= putMVar ret
    return False
  liftIO (takeMVar ret)

patch' :: Patchable widget => SomeState -> widget e1 -> widget e2 -> IO SomeState
patch' state markup1 markup2 = case patch state markup1 markup2 of
  Keep -> pure state
  Modify f -> f
  Replace f -> f
