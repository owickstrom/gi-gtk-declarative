{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

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
import           Data.Vector                             (Vector)
import qualified GI.Gtk                                  as Gtk
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
data CustomWidget widget params internalState event =
  CustomWidget
  { customWidget :: Gtk.ManagedPtr widget -> widget
  -- ^ The widget constructor
  , customCreate :: params -> IO (widget, internalState)
  -- ^ Action that creates the initial widget
  , customPatch :: params -> params -> internalState -> CustomPatch widget internalState
  -- ^ Patch function, calculating a 'CustomPatch' based on the state,
  -- old custom data, and new custom data
  , customSubscribe :: params -> internalState -> widget -> (event -> IO ()) -> IO Subscription
  -- ^ Action that creates an event subscription for the custom widget
  , customAttributes :: Vector (Attribute widget event)
  -- ^ Declarative 'Attribute's for the custom widget (properties and
  -- classes are handled automatically in patching)
  , customParams :: params
  -- ^ Parameters passed when constructing the declarative custom widget
  } deriving (Functor)

instance ( Typeable widget
         , Typeable internalState
         , Gtk.IsWidget widget
         )
  => Patchable (CustomWidget widget params internalState) where
  create custom = do
    (widget, internalState) <- customCreate custom (customParams custom)
    Gtk.widgetShow widget

    let collected = collectAttributes (customAttributes custom)
    updateProperties widget mempty (collectedProperties collected)
    sc <- Gtk.widgetGetStyleContext widget
    updateClasses sc mempty (collectedClasses collected)

    pure (SomeState (StateTreeWidget (StateTreeNode widget sc collected internalState)))
  patch (SomeState (stateTree :: StateTree st w e c cs)) old new =
    case (eqT @cs @internalState, eqT @widget @w) of
      (Just Refl, Just Refl) ->
        case customPatch
               new
               (customParams old)
               (customParams new)
               (stateTreeCustomState (stateTreeNode stateTree))
        of
          CustomReplace -> Replace (create new)
          CustomModify f -> Modify $ do
            let widget' = stateTreeNodeWidget stateTree

            let oldCollected = stateTreeCollectedAttributes (stateTreeNode stateTree)
                newCollected = collectAttributes (customAttributes new)
            updateProperties widget' (collectedProperties oldCollected) (collectedProperties newCollected)
            updateClasses (stateTreeStyleContext (stateTreeNode stateTree)) (collectedClasses oldCollected) (collectedClasses newCollected)
            internalState' <-
              f =<< Gtk.unsafeCastTo (customWidget new) widget'
            let node = stateTreeNode stateTree
            return (SomeState (StateTreeWidget node { stateTreeCustomState = internalState'
                                                    , stateTreeCollectedAttributes = newCollected
                                                    }))
          CustomKeep -> Keep
      _ -> Replace (create new)

instance (Typeable internalState, Gtk.GObject widget)
  => EventSource (CustomWidget widget params internalState) where
  subscribe custom (SomeState (stateTree :: StateTree st w e c cs)) cb =
    case eqT @cs @internalState of
      Just Refl -> do
        w' <- Gtk.unsafeCastTo (customWidget custom) (stateTreeNodeWidget stateTree)
        customSubscribe custom (customParams custom) (stateTreeCustomState (stateTreeNode stateTree)) w' cb
      Nothing -> pure (fromCancellation (pure ()))
