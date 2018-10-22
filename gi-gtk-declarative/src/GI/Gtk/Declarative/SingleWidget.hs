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
import           GI.Gtk.Declarative.Attributes.Collected
import           GI.Gtk.Declarative.Attributes.Internal
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Markup
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.State

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
    (SingleWidget ctor attrs) -> do
        let collected = collectAttributes attrs
        widget' <- Gtk.new ctor (constructProperties collected)
        sc <- Gtk.widgetGetStyleContext widget'
        updateClasses sc mempty (collectedClasses collected)
        -- TODO:
        -- mapM_ (applyAfterCreated widget') props

        Gtk.widgetShow widget'
        return (SomeState (StateTreeWidget (StateTreeNode widget' sc collected)))
  patch (SomeState (st :: StateTree stateType w child event))
        (SingleWidget (_    :: Gtk.ManagedPtr w1 -> w1) _)
        (SingleWidget (ctor :: Gtk.ManagedPtr w2 -> w2) newAttributes) =
    case (st, eqT @w @w1, eqT @w1 @w2) of
      (StateTreeWidget top, Just Refl, Just Refl) -> Modify $ do
        let w = stateTreeWidget top
        let oldCollected = stateTreeCollectedAttributes top
            newCollected = collectAttributes newAttributes
        updateProperties w (collectedProperties oldCollected) (collectedProperties newCollected)
        updateClasses (stateTreeStyleContext top) (collectedClasses oldCollected) (collectedClasses newCollected)
        return (SomeState (StateTreeWidget top { stateTreeCollectedAttributes = newCollected }))
      (_, _, _) -> Replace (create (SingleWidget ctor newAttributes))

instance EventSource (SingleWidget widget) where
  subscribe (SingleWidget (_ :: Gtk.ManagedPtr w1 -> w1) props) (SomeState (st :: StateTree stateType w2 child event)) cb = do
    case (st, eqT @w1 @w2) of
      (StateTreeWidget top, Just Refl) ->
        mconcat . catMaybes <$> mapM (addSignalHandler cb (stateTreeWidget top)) props
      _ -> fail ""

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
