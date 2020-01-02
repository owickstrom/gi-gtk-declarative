{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | A declarative representation of 'Gtk.Widget' in GTK without children.
module GI.Gtk.Declarative.SingleWidget
  ( SingleWidget
  , widget
  )
where

import           Data.Typeable
import           Data.Vector                    ( Vector )
import qualified GI.Gtk                        as Gtk

import           GI.Gtk.Declarative.Attributes
import           GI.Gtk.Declarative.Attributes.Collected
import           GI.Gtk.Declarative.Attributes.Internal
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.State
import           GI.Gtk.Declarative.Widget

-- | Declarative version of a /leaf/ widget, i.e. a widget without any children.
data SingleWidget widget event where
  SingleWidget
    ::(Typeable widget, Gtk.IsWidget widget, Functor (Attribute widget))
    => (Gtk.ManagedPtr widget -> widget)
    -> Vector (Attribute widget event)
    -> SingleWidget widget event

instance Functor (SingleWidget widget) where
  fmap f (SingleWidget ctor attrs) = SingleWidget ctor (fmap f <$> attrs)

instance Patchable (SingleWidget widget) where
  create = \case
    SingleWidget ctor attrs -> do
      let collected = collectAttributes attrs
      widget' <- Gtk.new ctor (constructProperties collected)
      Gtk.widgetShow widget'
      sc <- Gtk.widgetGetStyleContext widget'
      updateClasses sc mempty (collectedClasses collected)
      return
        (SomeState (StateTreeWidget (StateTreeNode widget' sc collected ())))
  patch (SomeState (st :: StateTree stateType w child event cs)) (SingleWidget (_ :: Gtk.ManagedPtr
      w1
    -> w1) _) (SingleWidget (ctor :: Gtk.ManagedPtr w2 -> w2) newAttributes)
    = case (st, eqT @w @w1, eqT @w1 @w2) of
      (StateTreeWidget top, Just Refl, Just Refl) ->
        let
          oldCollected      = stateTreeCollectedAttributes top
          newCollected      = collectAttributes newAttributes
          oldCollectedProps = collectedProperties oldCollected
          newCollectedProps = collectedProperties newCollected
        in
          if oldCollectedProps `canBeModifiedTo` newCollectedProps
            then Modify $ do
              let w = stateTreeWidget top
              updateProperties w oldCollectedProps newCollectedProps
              updateClasses (stateTreeStyleContext top)
                            (collectedClasses oldCollected)
                            (collectedClasses newCollected)
              let top' = top { stateTreeCollectedAttributes = newCollected }
              return
                (SomeState
                  (StateTreeWidget top'
                    { stateTreeCollectedAttributes = newCollected
                    }
                  )
                )
            else Replace (create (SingleWidget ctor newAttributes))
      _ -> Replace (create (SingleWidget ctor newAttributes))

instance EventSource (SingleWidget widget) where
  subscribe (SingleWidget (_ :: Gtk.ManagedPtr w1 -> w1) props) (SomeState (st :: StateTree
      stateType
      w2
      child
      event
      cs)) cb
    = case (st, eqT @w1 @w2) of
      (StateTreeWidget top, Just Refl) ->
        foldMap (addSignalHandler cb (stateTreeWidget top)) props
      _ -> pure (fromCancellation (pure ()))

-- instance (Typeable widget, Functor (SingleWidget widget))
--   => FromWidget (SingleWidget widget) Widget where
--   fromWidget = Widget

-- | Construct a /leaf/ widget, i.e. one without any children.
widget
  :: ( Typeable widget
     , Gtk.IsWidget widget
     , FromWidget (SingleWidget widget) target
     )
  => (Gtk.ManagedPtr widget -> widget) -- ^ A widget constructor from the underlying gi-gtk library.
  -> Vector (Attribute widget event)   -- ^ List of 'Attribute's.
  -> target event                      -- ^ The target, whose type is decided by 'FromWidget'.
widget ctor = fromWidget . SingleWidget ctor
