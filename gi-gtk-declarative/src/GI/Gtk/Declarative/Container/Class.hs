{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE TypeFamilies           #-}
module GI.Gtk.Declarative.Container.Class where

import qualified GI.Gtk                           as Gtk
import           Data.Vector                             (Vector)
import qualified Data.Vector                             as Vector
import           Data.Int                         (Int32)

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

newtype Children child event = Children { unChildren :: Vector (child event) }
  deriving (Functor)

class ToChildren widget parent child | widget -> parent, widget -> child where
  toChildren :: (Gtk.ManagedPtr widget -> widget) -> parent (child event) -> Children child event

  default toChildren :: parent ~ [] => (Gtk.ManagedPtr widget -> widget) -> parent (child event) -> Children child event
  toChildren _ = Children . Vector.fromList
