{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Shared interfaces for containers.
module GI.Gtk.Declarative.Container.Class where

import           Data.Int                       ( Int32 )
import           Data.Vector                    ( Vector )
import qualified GI.Gtk                        as Gtk

-- | Describes supported GTK+ containers and their specialized APIs for
-- appending and replacing child widgets.
class IsContainer container child | container -> child where
  -- | Append a child widget to the container.
  appendChild
    :: container    -- ^ Container widget
    -> child event  -- ^ Declarative child widget
    -> Gtk.Widget   -- ^ GTK child widget to append
    -> IO ()
  -- | Replace the child widget at the given index in the container.
  replaceChild
    :: container    -- ^ Container widget
    -> child event  -- ^ Declarative child widget
    -> Int32        -- ^ Index to replace at
    -> Gtk.Widget   -- ^ Old GTK widget to replace
    -> Gtk.Widget   -- ^ New GTK widget to replace with
    -> IO ()

-- | Common collection type for child widgets, used when patching containers.
newtype Children child event = Children { unChildren :: Vector (child event) }
  deriving (Functor)

-- | Converts a specific collection type to 'Children'.
class ToChildren widget parent child | widget -> parent, widget -> child where
  toChildren :: (Gtk.ManagedPtr widget -> widget) -> parent (child event) -> Children child event

  default toChildren :: parent ~ Vector => (Gtk.ManagedPtr widget -> widget) -> parent (child event) -> Children child event
  toChildren _ = Children
