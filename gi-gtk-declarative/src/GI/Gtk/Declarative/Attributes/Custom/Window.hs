{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module GI.Gtk.Declarative.Attributes.Custom.Window
  ( window
  , presentWindow
  , IconData(..)
  , windowIcon
  , setDefaultIcon
  ) where

import           Control.Monad                     (when)
import           Data.ByteString                   (ByteString)
import           Data.Hashable                     (Hashable)
import           Data.Typeable                     (Typeable)

import qualified GI.GdkPixbuf.Objects.Pixbuf       as Pixbuf
import qualified GI.GdkPixbuf.Objects.PixbufLoader as Pixbuf
import qualified GI.Gtk                            as Gtk

import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.State

-- | Construct a new declarative top-level window, with a lifecycle
-- tied to the widget the attribute is attached to.
--
-- The key should uniquely identify this window amongst all windows
-- attached to the same widget.
window
  :: (Typeable key, Eq key, Hashable key)
  => key
  -> Bin Gtk.Window event
  -> Attribute widget event
window key bin' = customAttribute key (Window bin')

newtype Window event = Window (Bin Gtk.Window event)
  deriving (Functor)

instance CustomAttribute widget Window where

  data AttrState Window = WindowState SomeState

  attrCreate _widget (Window bin') =
    WindowState <$> create bin'

  attrPatch _widget (WindowState state1) (Window bin1) (Window bin2) =
    case patch state1 bin1 bin2 of
      Keep -> pure $ WindowState state1
      Modify p -> WindowState <$> p
      Replace p -> do
        destroy state1 bin1
        WindowState <$> p

  attrDestroy _widget (WindowState state) (Window bin') =
    destroy state bin'

  attrSubscribe _widget (WindowState state) (Window bin') cb =
    subscribe bin' state cb

-- | Create an attribute for "presenting" (i.e. focusing) a window: when
-- the value changes then the window will be presented.
presentWindow :: (Eq a, Typeable a) => a -> Attribute Gtk.Window event
presentWindow state = customAttribute () (PresentWindow state)

newtype PresentWindow a event = PresentWindow a
  deriving (Functor)

instance (Typeable a, Eq a) => CustomAttribute Gtk.Window (PresentWindow a) where

  data AttrState (PresentWindow a) = PresentWindowState

  attrCreate _window (PresentWindow _) =
    pure PresentWindowState

  attrPatch window' PresentWindowState (PresentWindow a) (PresentWindow b) = do
    when (a /= b) $
      Gtk.windowPresent window'
    pure PresentWindowState

-- | Set the icon that is used for windows by default.
setDefaultIcon :: IconData -> IO ()
setDefaultIcon dat = do
  pixbuf <- loadPixbuf dat
  Gtk.windowSetDefaultIcon pixbuf

-- | Set the icon that is used by one particular window.
windowIcon :: IconData -> Attribute Gtk.Window event
windowIcon = customAttribute () . WindowIcon

newtype WindowIcon event = WindowIcon IconData
  deriving (Functor)

instance CustomAttribute Gtk.Window WindowIcon where

  data AttrState WindowIcon = WindowIconState

  attrCreate window' (WindowIcon dat) = do
    pixbuf <- loadPixbuf dat
    Gtk.windowSetIcon window' (Just pixbuf)
    pure WindowIconState

  attrPatch window' state (WindowIcon old) (WindowIcon new) = do
    when (old /= new) $ do
      pixbuf <- loadPixbuf new
      Gtk.windowSetIcon window' (Just pixbuf)
    pure state

data IconData
  = IconDataBytes ByteString
  -- ^ Icon data in an image format supported by GdkPixbuf (e.g. PNG, JPG)
  deriving (Eq)

loadPixbuf :: IconData -> IO Pixbuf.Pixbuf
loadPixbuf (IconDataBytes bs) = do
  loader <- Pixbuf.pixbufLoaderNew
  Pixbuf.pixbufLoaderWrite loader bs
  Pixbuf.pixbufLoaderClose loader
  Pixbuf.pixbufLoaderGetPixbuf loader >>= \case
    Nothing     -> error "Failed loading icon into pixbuf."
    Just pixbuf -> pure pixbuf
