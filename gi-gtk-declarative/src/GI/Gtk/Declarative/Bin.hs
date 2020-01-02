{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | A declarative representation of 'Gtk.Bin' in GTK.
module GI.Gtk.Declarative.Bin
  ( Bin(..)
  , bin
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


-- | Declarative version of a /bin/ widget, i.e. a widget with exactly one
-- child.
data Bin widget event where
  Bin
    ::( Typeable widget
       , Gtk.IsContainer widget
       , Gtk.IsBin widget
       , Gtk.IsWidget widget
       )
    => (Gtk.ManagedPtr widget -> widget)
    -> Vector (Attribute widget event)
    -> Widget event
    -> Bin widget event

instance Functor (Bin widget) where
  fmap f (Bin ctor attrs child) = Bin ctor (fmap f <$> attrs) (fmap f child)

-- | Construct a /bin/ widget, i.e. a widget with exactly one child.
bin
  :: ( Typeable widget
     , Gtk.IsContainer widget
     , Gtk.IsBin widget
     , Gtk.IsWidget widget
     , FromWidget (Bin widget) target
     )
  => (Gtk.ManagedPtr widget -> widget) -- ^ A bin widget constructor from the underlying gi-gtk library.
  -> Vector (Attribute widget event)   -- ^ List of 'Attribute's.
  -> Widget event                       -- ^ The bin's child widget
  -> target event                      -- ^ The target, whose type is decided by 'FromWidget'.
bin ctor attrs = fromWidget . Bin ctor attrs

--
-- Patchable
--

instance (Gtk.IsBin parent) => Patchable (Bin parent) where
  create (Bin (ctor :: Gtk.ManagedPtr w -> w) attrs child) = do
    let collected = collectAttributes attrs
    widget' <- Gtk.new ctor (constructProperties collected)
    Gtk.widgetShow widget'

    sc <- Gtk.widgetGetStyleContext widget'
    updateClasses sc mempty (collectedClasses collected)

    childState  <- create child
    childWidget <- someStateWidget childState
    maybe (pure ()) Gtk.widgetDestroy =<< Gtk.binGetChild widget'
    Gtk.containerAdd widget' childWidget
    return
      (SomeState
        (StateTreeBin (StateTreeNode widget' sc collected ()) childState)
      )

  patch (SomeState (st :: StateTree stateType w1 c1 e1 cs)) (Bin _ _ oldChild) (Bin (ctor :: Gtk.ManagedPtr
      w2
    -> w2) newAttributes newChild)
    = case (st, eqT @w1 @w2) of
      (StateTreeBin top oldChildState, Just Refl) ->
        let
          oldCollected      = stateTreeCollectedAttributes top
          newCollected      = collectAttributes newAttributes
          oldCollectedProps = collectedProperties oldCollected
          newCollectedProps = collectedProperties newCollected
        in
          if oldCollectedProps `canBeModifiedTo` newCollectedProps
            then Modify $ do
              binWidget <- Gtk.unsafeCastTo ctor (stateTreeWidget top)
              updateProperties binWidget oldCollectedProps newCollectedProps
              updateClasses (stateTreeStyleContext top)
                            (collectedClasses oldCollected)
                            (collectedClasses newCollected)

              let top' = top { stateTreeCollectedAttributes = newCollected }
              case patch oldChildState oldChild newChild of
                Modify  modify    -> SomeState . StateTreeBin top' <$> modify
                Replace createNew -> do
                  Gtk.widgetDestroy =<< someStateWidget oldChildState
                  newChildState <- createNew
                  childWidget   <- someStateWidget newChildState
                  Gtk.widgetShow childWidget
                  maybe (pure ()) Gtk.widgetDestroy
                    =<< Gtk.binGetChild binWidget
                  Gtk.containerAdd binWidget childWidget
                  return (SomeState (StateTreeBin top' newChildState))
                Keep -> return (SomeState st)
            else Replace (create (Bin ctor newAttributes newChild))
      _ -> Replace (create (Bin ctor newAttributes newChild))

--
-- EventSource
--

instance Gtk.IsBin parent => EventSource (Bin parent) where
  subscribe (Bin ctor props child) (SomeState st) cb = case st of
    StateTreeBin top childState -> do
      binWidget <- Gtk.unsafeCastTo ctor (stateTreeWidget top)
      handlers' <- foldMap (addSignalHandler cb binWidget) props
      (<> handlers') <$> subscribe child childState cb
    _ -> error "Cannot subscribe to Bin events with a non-bin state tree."

instance a ~ b => FromWidget (Bin a) (Bin b) where
  fromWidget = id
