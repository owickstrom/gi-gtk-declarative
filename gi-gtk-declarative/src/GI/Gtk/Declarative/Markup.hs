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
  , Patchable(..)
  , MarkupBuilder
  , MarkupOf
  , Markup
  , widget
  , widgets
  , runMarkup
  , FromWidget(..)
  )
where

import           Control.Monad.Writer
import           Data.Typeable

import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Patch

-- | A 'Widget' value wraps a 'Patchable' and 'EventSource' widget, providing
-- a constrained equivalent of a 'Dynamic' value. It is used to support
-- heterogeneous containers of widgets, and to support equality
-- checks on different types of widgets when calculating patches.
data Widget event where
  Widget
    :: ( Typeable widget
       , Typeable event
       , Patchable widget
       , EventSource (widget event) event
       )
    => widget event
    -> Widget event

-- | 'Widget' is itself patchable, by delegating to the underlying
-- widget instances.
instance Patchable Widget where
  create (Widget w) = create w
  patch (Widget (w1 :: t1 e1)) (Widget (w2 :: t2 e2)) =
    case eqT @(t1 e1) @(t2 e2) of
      Just Refl -> patch w1 w2
      _         -> Replace (create w2)

instance EventSource (Widget event) event where
  subscribe (Widget w) = subscribe w

-- * Markup
newtype MarkupBuilder widget event a =
  MarkupBuilder (Writer [widget event] a)
  deriving (Functor, Applicative, Monad)

runMarkup :: MarkupOf widget event () -> [widget event]
runMarkup (MarkupBuilder w) = execWriter w

type MarkupOf widget event a = MarkupBuilder widget event a

type Markup event a = MarkupBuilder Widget event a

widget :: widget event -> MarkupOf widget event ()
widget w = MarkupBuilder (tell [w])

widgets :: [widget event] -> MarkupOf widget event ()
widgets = MarkupBuilder . tell

class FromWidget t event | t -> event where
  fromWidget :: Typeable event => Widget event -> t

instance FromWidget (MarkupOf Widget event ()) event where
  fromWidget = widget

instance FromWidget (Widget event) event where
  fromWidget = id
