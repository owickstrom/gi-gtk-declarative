{-# LANGUAGE MultiParamTypeClasses #-}
-- | A 'Patch' represents a possible 'IO' action to apply to a
-- 'Gtk.Widget' to make it reflect an updated declarative widget.  The
-- action to apply is calculated from the difference between the old
-- and the new declarative widget.

module GI.Gtk.Declarative.Patch
  ( Patch(..)
  , Patchable(..)
  ) where

import           Data.Typeable
import qualified GI.Gtk as Gtk

data Patch
  = Modify (Gtk.Widget -> IO ())
  | Replace (IO Gtk.Widget)
  | Keep

class Patchable widget where
  create :: Typeable event => widget event -> IO Gtk.Widget
  patch :: Typeable event => widget event -> widget event -> Patch
