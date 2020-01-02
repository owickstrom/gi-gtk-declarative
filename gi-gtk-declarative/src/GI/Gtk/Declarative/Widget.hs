{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}

-- | A 'Widget' value can wrap any 'Patchable' widget, hiding the underlying
-- widget type, such that you can embed heterogeneous collections of widgets in
-- containers.
module GI.Gtk.Declarative.Widget
  ( Widget(..)
  -- * Widget to Markup conversion
  , FromWidget(..)
  )
where

import           Data.Typeable

import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Patch

-- | A 'Widget' value wraps a 'Patchable' and 'EventSource' widget, providing
-- a constrained equivalent of a 'Dynamic' value. It is used to support
-- heterogeneous containers of widgets, and to support equality
-- checks on different types of widgets when calculating patches.
data Widget event where
  Widget
    ::( Typeable widget
       , Patchable widget
       , Functor widget
       , EventSource widget
       )
    => widget event
    -> Widget event

instance Functor Widget where
  fmap f (Widget w) = Widget (fmap f w)

-- | 'Widget' is itself patchable, by delegating to the underlying
-- widget instances.
instance Patchable Widget where
  create (Widget w) = create w
  patch s (Widget (w1 :: t1 e1)) (Widget (w2 :: t2 e2)) = case eqT @t1 @t2 of
    Just Refl -> patch s w1 w2
    _         -> Replace (create w2)

instance EventSource Widget where
  subscribe (Widget w) = subscribe w

-- | Convert a widget to a target type. This is deliberately unconstrained in
-- it's types, and is used by smart constructors to implement return type
-- polymorphism, so that a smart contructor can return either a 'Widget', or
-- some specifically typed widget, depending on the context in which it's
-- used.
class FromWidget widget target where
  fromWidget :: widget event -> target event

instance ( Typeable parent
         , Typeable child
         , Patchable (parent child)
         , Functor (parent child)
         , EventSource (parent child)
         )
         => FromWidget (parent child) Widget where
  fromWidget = Widget
