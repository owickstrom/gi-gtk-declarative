{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module GI.Gtk.Declarative.Patch
  ( Patch(..)
  , Patchable(..)
  ) where

import qualified GI.Gtk as Gtk

-- | A possible action to take on an existing 'Gtk.Widget', decided by the
-- 'patch' method when comparing declarative widgets.
data Patch
  = Modify (Gtk.Widget -> IO ())
  -- ^ An 'IO' action to apply to a 'Gtk.Widget' to make it reflect an updated
  -- declarative widget. The action to apply is calculated from the difference
  -- between the old and the new declarative widget.
  | Replace (IO Gtk.Widget)
  -- ^ Replace the current 'Gtk.Widget' by the widget returned by the IO
  -- action.
  | Keep
  -- ^ Do nothing, i.e. keep the 'Gtk.Widget' as it is.

-- | A patchable widget is one that can create an underlying GTK widget, or
-- calculate a 'Patch' to be applied to an existing GTK widget that was
-- previously created.
class Patchable widget where
  -- | Given a declarative widget that is 'Patchable', return an IO action that
  -- can create a new corresponding 'Gtk.Widget'. The created widget should be
  -- use in corresponding patch modifications, until it is replaced.
  create :: widget e -> IO Gtk.Widget
  -- | Given two declarative widgets of the same widget type (but not
  -- necessarily of the same event types,) calculate a 'Patch'.
  patch :: widget e1 -> widget e2 -> Patch
