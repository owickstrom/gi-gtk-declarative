{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- | 'Markup' is the common type for the (declarative) GTK+ markup
-- language. A 'Markup' value can wrap a 'Patchable' widgets, and
-- embedded in a container together with 'Markup' values for different
-- types of widgets. In other words, containers can be heterogeneous.
module GI.Gtk.Declarative.Markup where

import           Data.Typeable

import           GI.Gtk.Declarative.Patch

-- | A 'Markup' value wraps a 'Patchable' widget, providing a
-- constrained equivalent of a 'Dynamic' value. It is used to support
-- heterogeneous containers of widgets, and to support equality
-- checks on different types of widgets when calculating patches.
data Markup e where
  Markup
    :: (Typeable (widget event), Patchable widget event)
    => widget event
    -> Markup event

-- | 'Markup' is itself patchable, by delegating to the underlying
-- widget instances.
instance Patchable Markup event where
  create (Markup w) = create w
  patch (Markup (w1 :: t1 e)) (Markup (w2 :: t2 e)) =
    case eqT @(t1 e) @(t2 e) of
      Just Refl -> patch w1 w2
      Nothing   -> Replace (create w2)
