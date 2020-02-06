{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module GI.Gtk.Declarative.Attributes.Custom.Window
  ( window
  , presentWindow
  ) where

import           Control.Monad                        (when)
import           Data.Typeable                        (Typeable)

import qualified GI.Gtk                               as Gtk

import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Attributes.Custom
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.State

-- | Construct a new declarative top-level window, with a lifecycle
-- tied to the widget the attribute is attached to
window :: Bin Gtk.Window event -> Attribute widget event
window bin = customAttribute $ Window bin

data Window event = Window (Bin Gtk.Window event)
  deriving (Functor)

instance CustomAttribute widget Window where

  data State Window = WindowState SomeState

  attrCreate _widget (Window bin) =
    WindowState <$> create bin

  attrPatch _widget (WindowState state1) (Window bin1) (Window bin2) =
    case patch state1 bin1 bin2 of
      Keep -> pure $ WindowState state1
      Modify p -> WindowState <$> p
      Replace p -> do
        destroy state1 bin1
        WindowState <$> p

  attrDestroy _widget (WindowState state) (Window bin) = do
    destroy state bin

  attrSubscribe _widget (WindowState state) (Window bin) cb =
    subscribe bin state cb

-- | Create an attribute for "presenting" (i.e. focusing) a window: when
-- the value changes then the window will be presented.
presentWindow :: (Eq a, Typeable a) => a -> Attribute Gtk.Window event
presentWindow state = customAttribute $ PresentWindow state

data PresentWindow a event = PresentWindow a
  deriving (Functor)

instance (Typeable a, Eq a) => CustomAttribute Gtk.Window (PresentWindow a) where

  data State (PresentWindow a) = PresentWindowState

  attrCreate _window (PresentWindow a) =
    pure PresentWindowState

  attrPatch window PresentWindowState (PresentWindow a) (PresentWindow b) = do
    when (a /= b) $ do
      Gtk.windowPresent window
    pure PresentWindowState

  attrDestroy _window PresentWindowState (PresentWindow _) =
    mempty

  attrSubscribe _window PresentWindowState (PresentWindow _) _cb =
    mempty
