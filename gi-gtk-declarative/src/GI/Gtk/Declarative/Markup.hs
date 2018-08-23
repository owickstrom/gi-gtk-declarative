{-# LANGUAGE GADTs                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}

-- | 'Markup' is the common type for the (declarative) GTK+ markup
-- language, supporting do syntax. A 'SomeWidget' value can wrap a 'Patchable'
-- widgets, and embedded in a container together with 'SomeWidget' values for
-- different types of widgets. In other words, containers can be heterogeneous.
module GI.Gtk.Declarative.Markup where

import           Data.Typeable

import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.EventSource

-- | A 'SomeWidget' value wraps a 'Patchable' widget, providing a
-- constrained equivalent of a 'Dynamic' value. It is used to support
-- heterogeneous containers of widgets, and to support equality
-- checks on different types of widgets when calculating patches.
data Markup event where
  Markup
    :: (Typeable (widget event), Patchable (widget event), EventSource (widget event) event)
    => widget event
    -> Markup event

-- | 'Markup' is itself patchable, by delegating to the underlying
-- widget instances.

instance Typeable event => Patchable (Markup event) where
  create (Markup w) = create w
  patch
    (Markup (w1 :: t1 event))
    (Markup (w2 :: t2 event)) =
      case eqT @(t1 event) @(t2 event) of
        Just Refl -> patch w1 w2
        _   -> Replace (create w2)

instance EventSource (Markup event) event where
  subscribe (Markup w) = subscribe w
