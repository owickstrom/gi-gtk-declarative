{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

-- | A declarative representation of 'Gtk.Widget' in GTK without children.
module GI.Gtk.Declarative.SingleWidget
  ( SingleWidget
  , widget
  )
where

import           Data.Maybe
import           Data.Typeable
import qualified GI.Gtk                         as Gtk

import           GI.Gtk.Declarative.Attributes
import           GI.Gtk.Declarative.Attributes.Internal
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Markup
import           GI.Gtk.Declarative.Patch

-- | Declarative version of a /leaf/ widget, i.e. a widget without any children.
data SingleWidget widget event where
  SingleWidget
    :: (Typeable widget, Gtk.IsWidget widget, Functor (Attribute widget))
    => (Gtk.ManagedPtr widget -> widget)
    -> [Attribute widget event]
    -> SingleWidget widget event

instance Functor (SingleWidget widget) where
  fmap f (SingleWidget ctor attrs) = SingleWidget ctor (fmap f <$> attrs)

instance Patchable (SingleWidget widget) where
  create = \case
    (SingleWidget ctor props) -> do
        let attrOps = concatMap extractAttrConstructOps props
        widget' <- Gtk.new ctor attrOps

        sc <- Gtk.widgetGetStyleContext widget'
        mapM_ (addClass sc) props
        mapM_ (applyAfterCreated widget') props

        Gtk.widgetShowAll widget'
        Gtk.toWidget widget'
  patch (SingleWidget (_    :: Gtk.ManagedPtr w1 -> w1) oldAttributes)
        (SingleWidget (ctor :: Gtk.ManagedPtr w2 -> w2) newAttributes) =
    case eqT @w1 @w2 of
      Just Refl ->
        Modify $ \widget' -> do
          w <- Gtk.unsafeCastTo ctor widget'
          Gtk.set w (concatMap extractAttrSetOps newAttributes)

          sc <- Gtk.widgetGetStyleContext widget'
          mapM_ (removeClass sc) oldAttributes
          mapM_ (addClass sc) newAttributes

          Gtk.widgetShowAll w

      Nothing -> Replace (create (SingleWidget ctor newAttributes))

instance EventSource (SingleWidget widget) where
  subscribe (SingleWidget ctor props) widget' cb = do
    w <- Gtk.unsafeCastTo ctor widget'
    mconcat . catMaybes <$> mapM (addSignalHandler cb w) props

instance (Typeable widget, Functor (SingleWidget widget))
  => FromWidget (SingleWidget widget) event (Widget event) where
  fromWidget = Widget

instance FromWidget (SingleWidget widget) event (MarkupOf (SingleWidget widget) event ()) where
  fromWidget = single

instance (Typeable widget, Functor (SingleWidget widget))
  => FromWidget (SingleWidget widget) event (Markup event ()) where
  fromWidget = single . Widget

-- | Construct a /leaf/ widget, i.e. one without any children.
widget ::
     ( Typeable widget
     , Typeable event
     , Functor (Attribute widget)
     , Gtk.IsWidget widget
     , FromWidget (SingleWidget widget) event target
     )
  => (Gtk.ManagedPtr widget -> widget) -- ^ A widget constructor from the underlying gi-gtk library.
  -> [Attribute widget event]          -- ^ List of 'Attribute's.
  -> target                            -- ^ The target, whose type is decided by 'FromWidget'.
widget ctor = fromWidget . SingleWidget ctor
