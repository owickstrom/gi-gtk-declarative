{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}

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
data Markup event where
  Markup
    :: (Functor widget, Typeable widget, Patchable widget)
    => widget event
    -> Markup event

instance Functor Markup where
  fmap f (Markup w) = Markup (fmap f w)

-- | 'Markup' is itself patchable, by delegating to the underlying
-- widget instances.

instance Patchable Markup where
  create (Markup w) = create w
  patch
    (Markup (w1 :: t1 event))
    (Markup (w2 :: t2 event)) =
      case eqT @(t1 event) @(t2 event) of
        Just Refl -> patch w1 w2
        _   -> Replace (create w2)

-- mapP :: (Typeable b) => (event -> b) -> widget event -> widget b
-- mapP p f (Markup (w :: widget event)) = Markup (mapP (Proxy :: Proxy widget) f w)
