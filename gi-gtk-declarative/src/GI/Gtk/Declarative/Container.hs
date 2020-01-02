{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

-- | Implementations for common "Gtk.Container".
module GI.Gtk.Declarative.Container
  ( Container
  , container
  , Children
  , ToChildren(..)
  )
where

import           Control.Monad                  ( forM )
import           Data.Typeable
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as Vector
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative.Attributes
import           GI.Gtk.Declarative.Attributes.Collected
import           GI.Gtk.Declarative.Attributes.Internal
import           GI.Gtk.Declarative.Container.Class
import           GI.Gtk.Declarative.Container.Patch
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.State
import           GI.Gtk.Declarative.Widget

-- | Declarative version of a /container/ widget, i.e. a widget with zero
-- or more child widgets. The type of 'children' is parameterized, and differs
-- across the supported container widgets, as some containers require specific
-- types of child widgets. These type relations are decided by 'IsContainer',
-- and instances can found in "GI.Gtk.Declarative.Container.Patch".
data Container widget children event where
  Container ::( Typeable widget,
      Gtk.IsWidget widget,
      Gtk.IsContainer widget,
      Functor children
    ) =>
    (Gtk.ManagedPtr widget -> widget) ->
    Vector (Attribute widget event) ->
    children event ->
    Container widget children event

instance Functor (Container widget children) where
  fmap f (Container ctor attrs children) =
    Container ctor (fmap f <$> attrs) (fmap f children)

-- | Construct a /container/ widget, i.e. a widget with zero or more children.
container
  :: ( Typeable widget
     , Functor child
     , Gtk.IsWidget widget
     , Gtk.IsContainer widget
     , FromWidget (Container widget (Children child)) target
     , ToChildren widget parent child
     )
  => 
  -- | A container widget constructor from the underlying gi-gtk library.
     (Gtk.ManagedPtr widget -> widget)
  ->
  -- | 'Attribute's.
     Vector (Attribute widget event)
  ->
  -- | The container's 'child' widgets, in a 'MarkupOf' builder.
     parent (child event)
  ->
  -- | The target, whose type is decided by 'FromWidget'.
     target event
container ctor attrs = fromWidget . Container ctor attrs . toChildren ctor

--
-- Patchable
--

instance
  (Patchable child, Typeable child, IsContainer container child) =>
  Patchable (Container container (Children child))
  where

  create (Container ctor attrs children) = do
    let collected = collectAttributes attrs
    widget' <- Gtk.new ctor (constructProperties collected)
    Gtk.widgetShow widget'
    sc <- Gtk.widgetGetStyleContext widget'
    updateClasses sc mempty (collectedClasses collected)
    childStates <- forM (unChildren children) $ \child -> do
      childState <- create child
      appendChild widget' child =<< someStateWidget childState
      return childState
    return
      (SomeState
        (StateTreeContainer (StateTreeNode widget' sc collected ()) childStates)
      )

  patch (SomeState (st :: StateTree stateType w1 c1 e1 cs)) (Container _ _ oldChildren) new@(Container (ctor :: Gtk.ManagedPtr
      w2
    -> w2) newAttributes (newChildren :: Children c2 e2))
    = case (st, eqT @w1 @w2) of
      (StateTreeContainer top childStates, Just Refl) ->
        let oldCollected      = stateTreeCollectedAttributes top
            newCollected      = collectAttributes newAttributes
            oldCollectedProps = collectedProperties oldCollected
            newCollectedProps = collectedProperties newCollected
        in  if oldCollectedProps `canBeModifiedTo` newCollectedProps
              then Modify $ do
                containerWidget <- Gtk.unsafeCastTo ctor (stateTreeWidget top)
                updateProperties containerWidget
                                 oldCollectedProps
                                 newCollectedProps
                updateClasses (stateTreeStyleContext top)
                              (collectedClasses oldCollected)
                              (collectedClasses newCollected)
                let top' = top { stateTreeCollectedAttributes = newCollected }
                SomeState <$> patchInContainer
                  (StateTreeContainer top' childStates)
                  containerWidget
                  (unChildren oldChildren)
                  (unChildren newChildren)
              else Replace (create new)
      _ -> Replace (create new)

--
-- EventSource
--

instance
  EventSource child =>
  EventSource (Container widget (Children child))
  where
  subscribe (Container ctor props children) (SomeState st) cb = case st of
    StateTreeContainer top childStates -> do
      parentWidget <- Gtk.unsafeCastTo ctor (stateTreeWidget top)
      handlers' <- foldMap (addSignalHandler cb parentWidget) props
      subs <- flip foldMap (Vector.zip (unChildren children) childStates)
        $ \(c, childState) -> subscribe c childState cb
      return (handlers' <> subs)
    _ ->
      error
        "Warning: Cannot subscribe to Container events with a non-container state tree."

--
-- FromWidget
--

instance
  ( Typeable widget,
    Typeable children,
    Patchable (Container widget children),
    EventSource (Container widget children)
  ) =>
  FromWidget (Container widget children) Widget
  where
  fromWidget = Widget

instance a ~ b => FromWidget (Container a children) (Container b children) where
  fromWidget = id
