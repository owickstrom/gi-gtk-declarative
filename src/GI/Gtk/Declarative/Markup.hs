{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
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
data Markup where
  Markup :: (Eq w, Show w, Typeable w, Patchable w) => w -> Markup

-- | 'Markup' is itself patchable, by delegating to the underlying
-- widget instances.
instance Patchable Markup where
  create (Markup w) = create w
  patch (Markup (w1 :: t1)) (Markup (w2 :: t2)) =
    case eqT @t1 @t2 of
      Just Refl -> patch w1 w2
      Nothing   -> Replace (create w2)

deriving instance Show Markup

instance Eq Markup where
  Markup (a :: x) == Markup (b :: y) =
    case eqT @x @y  of
      Just Refl -> a == b
      Nothing   -> False
