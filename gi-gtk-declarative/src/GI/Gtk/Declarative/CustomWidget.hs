{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | While you can instantiate 'Patchable' and 'EventSource' for your
-- own data types, it's a bit complicated. The 'CustomWidget' data
-- type takes care of some lower-level detail, so that you can focus
-- on the custom behavior of your widget. You still need to think
-- about and implement a patching function, but in an easier way.
module GI.Gtk.Declarative.CustomWidget
  ( CustomPatch(..)
  , CustomWidget(..)
  )
where

import           Data.Typeable
import           Data.Vector                    ( Vector )
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative.Attributes
import           GI.Gtk.Declarative.Attributes.Collected
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.State

-- | Similar to 'Patch', describing a possible action to perform on a
-- 'Gtk.Widget', decided by 'customPatch'.
data CustomPatch widget internalState
  = CustomReplace
  | CustomModify (widget -> IO internalState)
  | CustomKeep

-- | A custom widget specification, with all functions needed to
-- instantiate 'Patchable' and 'EventSource'. A custom widget:
--
-- * is based on a top 'widget'
-- * can use 'internalState' as a way of keeping an internal state
--   value threaded through updates, which is often useful for passing
--   references to child widgets used in a custom widget
-- * emits events of type 'event'
data CustomWidget widget params internalState event
  = CustomWidget
      { -- | The widget constructor
        customWidget :: Gtk.ManagedPtr widget -> widget,
        -- | Action that creates the initial widget
        customCreate :: params -> IO (widget, internalState),
        -- | Patch function, calculating a 'CustomPatch' based on the state,
        -- old custom data, and new custom data
        customPatch :: params -> params -> internalState -> CustomPatch widget internalState,
        -- | Action that creates an event subscription for the custom widget
        customSubscribe :: params -> internalState -> widget -> (event -> IO ()) -> IO Subscription,
        -- | Declarative 'Attribute's for the custom widget (properties and
        -- classes are handled automatically in patching)
        customAttributes :: Vector (Attribute widget event),
        -- | Parameters passed when constructing the declarative custom widget
        customParams :: params
      }
  deriving (Functor)

instance
  ( Typeable widget,
    Typeable internalState,
    Gtk.IsWidget widget
  ) =>
  Patchable (CustomWidget widget params internalState)
  where

  create custom = do
    (widget, internalState) <- customCreate custom (customParams custom)
    Gtk.widgetShow widget
    let collected = collectAttributes (customAttributes custom)
    updateProperties widget mempty (collectedProperties collected)
    sc <- Gtk.widgetGetStyleContext widget
    updateClasses sc mempty (collectedClasses collected)
    ca <- createCustomAttributes widget (customAttributes custom)
    pure
      (SomeState
        (StateTreeWidget (StateTreeNode widget sc collected ca internalState))
      )

  patch (SomeState (stateTree :: StateTree st w c e cs)) old new =
    case (eqT @cs @internalState, eqT @widget @w) of
      (Just Refl, Just Refl) ->
        let
          node = stateTreeNode stateTree
          oldCollected = stateTreeCollectedAttributes node
          newCollected = collectAttributes (customAttributes new)
          oldCollectedProps = collectedProperties oldCollected
          newCollectedProps = collectedProperties newCollected
          oldCustomAttributeStates = stateTreeCustomAttributeStates node
          canBeModified = oldCollectedProps `canBeModifiedTo` newCollectedProps
        in
          case
            customPatch new
                        (customParams old)
                        (customParams new)
                        (stateTreeCustomState (stateTreeNode stateTree))
          of
            CustomReplace -> Replace (create new)
            p
              | canBeModified -> Modify $ do
                let widget' = stateTreeNodeWidget stateTree
                updateProperties widget' oldCollectedProps newCollectedProps
                updateClasses
                  (stateTreeStyleContext (stateTreeNode stateTree))
                  (collectedClasses oldCollected)
                  (collectedClasses newCollected)
                newCustomAttributeStates <- patchCustomAttributes widget' oldCustomAttributeStates (customAttributes old) (customAttributes new)
                internalState' <- case p of
                  CustomModify f ->
                    f =<< Gtk.unsafeCastTo (customWidget new) widget'
                  CustomKeep    -> pure (stateTreeCustomState node)
                  CustomReplace -> pure (stateTreeCustomState node) -- already handled above
                return
                  (SomeState
                    (StateTreeWidget node
                      { stateTreeCustomState           = internalState'
                      , stateTreeCollectedAttributes   = newCollected
                      , stateTreeCustomAttributeStates = newCustomAttributeStates
                      }
                    )
                  )
              | otherwise -> Replace (create new)
      _ -> Replace (create new)
  
  destroy (SomeState (st :: StateTree st w c e cs)) custom = do
    case (st, eqT @w @widget) of
      (StateTreeWidget StateTreeNode {..}, Just Refl) -> do
        destroyCustomAttributes stateTreeWidget stateTreeCustomAttributeStates (customAttributes custom)
        Gtk.widgetDestroy stateTreeWidget
      _ -> error "CustomWidget destroy called with invalid types"

instance
  (Typeable internalState, Typeable widget, Gtk.GObject widget) =>
  EventSource (CustomWidget widget params internalState)
  where
  subscribe custom (SomeState (st :: StateTree st w c e cs)) cb =
    case (st, eqT @w @widget, eqT @cs @internalState) of
      (StateTreeWidget StateTreeNode {..}, Just Refl, Just Refl) -> do
        w' <- Gtk.unsafeCastTo (customWidget custom) stateTreeWidget
        customSubscribe custom (customParams custom) stateTreeCustomState w' cb
          <> subscribeCustomAttributes stateTreeWidget stateTreeCustomAttributeStates (customAttributes custom) cb
      _ -> pure (fromCancellation (pure ()))
