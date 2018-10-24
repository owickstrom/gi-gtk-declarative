{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

-- | 'MarkupOf' is the common builder type for the (declarative) GTK+ markup
-- language, supporting do syntax. 'MarkupOf' wraps many widgets, usually of
-- type 'Widget', but they can be of other types.
--
-- A 'Widget' value can wrap any 'Patchable' widget, hiding the underlying
-- widget type, such that you can embed heterogeneous collections of widgets in
-- containers.
module GI.Gtk.Declarative.Markup
  ( Widget(..)
  -- * Markup
  , MarkupOf
  , Markup
  , single
  , multiple
  , runMarkup
  -- * Widget to Markup conversion
  , FromWidget(..)
  )
where

import           Control.Monad.Writer
import           Data.Typeable
import           Data.Vector                    (Vector)

import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Patch

-- | A 'Widget' value wraps a 'Patchable' and 'EventSource' widget, providing
-- a constrained equivalent of a 'Dynamic' value. It is used to support
-- heterogeneous containers of widgets, and to support equality
-- checks on different types of widgets when calculating patches.
data Widget event where
  Widget
    :: ( Typeable widget
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
  patch s (Widget (w1 :: t1 e1)) (Widget (w2 :: t2 e2)) =
    case eqT @t1 @t2 of
      Just Refl -> patch s w1 w2
      _         -> Replace (create w2)

instance EventSource Widget where
  subscribe (Widget w) = subscribe w

-- * Markup

-- | The declarative markup builder, primarily for using its 'Monad' instance
-- and do notation to construct adjacent widgets in containers.
--
-- It is parameterized with 'widget' and 'event', such that containers can
-- restrict the type of their children to other types than 'Widget'.
--
-- Note that the return type, 'a', is not used in this library. It's a more a
-- technical necessity to have the 'Monad' instance. You can still use it if
-- you need to return a value from a markup function, though.
newtype MarkupOf widget event a =
  MarkupOf (Writer (Vector (widget event)) a)
  deriving (Functor, Applicative, Monad)

-- | Run a 'MarkupOf' builder and get its widgets.
runMarkup :: MarkupOf widget event () -> Vector (widget event)
runMarkup (MarkupOf w) = execWriter w

-- | Handy type alias for the common case of markup containing 'Widget's.
type Markup event a = MarkupOf Widget event a

-- | Construct markup from a single widget.
single :: widget event -> MarkupOf widget event ()
single = MarkupOf . tell . pure

-- | Construct markup from multiple widgets.
multiple :: Vector (widget event) -> MarkupOf widget event ()
multiple = MarkupOf . tell

-- | Convert a widget to a target type. This is deliberately unconstrained in
-- it's types, and is used by smart constructors to implement return type
-- polymorphism, so that a smart contructor can return either a 'Widget', or
-- some specifically typed 'MarkupOf', depending on the context in which it's
-- used.
class FromWidget widget event target | target -> event where
  fromWidget :: (Typeable widget, Typeable event) => widget event -> target

instance FromWidget Widget event (Widget event) where
  fromWidget = id
