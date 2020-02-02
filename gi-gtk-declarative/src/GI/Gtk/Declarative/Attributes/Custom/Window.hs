{-# LANGUAGE RecordWildCards              #-}

module GI.Gtk.Declarative.Attributes.Custom.Window (presentWindow) where

import Control.Monad (when)
import Data.Typeable (Typeable)

import qualified GI.Gtk                        as Gtk

import GI.Gtk.Declarative.Attributes
import GI.Gtk.Declarative.Attributes.Custom

newtype PresentWindow a = PresentWindow a
  deriving (Eq)

-- todo: move to own module
-- todo: use typeclases so there is a single patch function per state type
presentWindow :: (Eq a, Typeable a) => a -> Attribute Gtk.Window event
presentWindow state = Custom $ CustomAttribute {..}
  where
    attrCreate _window =
        pure $ PresentWindow state
    
    attrPatch window (PresentWindow newState) = do
        when (newState /= state) $ do
            Gtk.windowPresent window -- todo: use windowPresentWithTime
        pure $ PresentWindow newState
    
    attrDestroy _window _state =
        pure ()