{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

-- | A declarative representation of 'Gtk.Bin' in GTK.
module GI.Gtk.Declarative.Bin
  ( Bin(..)
  , bin
  , BinChild
  )
where

import           Data.Typeable
import           Data.Vector                             (Vector)
import qualified GI.Gtk                                  as Gtk

import           GI.Gtk.Declarative.Attributes
import           GI.Gtk.Declarative.Attributes.Collected
import           GI.Gtk.Declarative.Attributes.Internal
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Markup
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.State


-- | Supported 'Gtk.Bin's.
class BinChild bin (child :: * -> *) | bin -> child

instance BinChild Gtk.ScrolledWindow Widget where
instance BinChild Gtk.ListBoxRow Widget where
instance BinChild Gtk.Window Widget where
instance BinChild Gtk.Dialog Widget where
instance BinChild Gtk.MenuItem Widget where

-- | Declarative version of a /bin/ widget, i.e. a widget with exactly one
-- child.
data Bin widget child event where
  Bin
    :: ( Typeable widget
       , Gtk.IsContainer widget
       , Gtk.IsBin widget
       , Gtk.IsWidget widget
       , Functor child
       )
    => (Gtk.ManagedPtr widget -> widget)
    -> Vector (Attribute widget event)
    -> child event
    -> Bin widget child event

instance Functor (Bin widget child) where
  fmap f (Bin ctor attrs child) =
    Bin ctor (fmap f <$> attrs) (fmap f child)

-- | Construct a /bin/ widget, i.e. a widget with exactly one child.
bin
  :: ( Patchable (Bin widget child)
     , Typeable widget
     , Typeable child
     , Typeable event
     , Functor child
     , Gtk.IsContainer widget
     , Gtk.IsBin widget
     , Gtk.IsWidget widget
     , FromWidget (Bin widget child) event target
     )
  => (Gtk.ManagedPtr widget -> widget) -- ^ A bin widget constructor from the underlying gi-gtk library.
  -> Vector (Attribute widget event)   -- ^ List of 'Attribute's.
  -> child event                       -- ^ The bin's child widget, whose type is decided by the 'BinChild' instance.
  -> target                            -- ^ The target, whose type is decided by 'FromWidget'.
bin ctor attrs = fromWidget . Bin ctor attrs

--
-- Patchable
--

instance (BinChild parent child, Patchable child) => Patchable (Bin parent child) where
  create (Bin ctor attrs child) = do
    let collected = collectAttributes attrs
    widget' <- Gtk.new ctor (constructProperties collected)
    Gtk.widgetShow widget'

    sc <- Gtk.widgetGetStyleContext widget'
    updateClasses sc mempty (collectedClasses collected)

    -- TODO:
    -- mapM_ (applyAfterCreated widget') props

    childState <- create child
    childWidget <- someStateWidget childState
    Gtk.containerAdd widget' childWidget
    return (SomeState (StateTreeBin (StateTreeNode widget' sc collected ()) childState))

  patch (SomeState (st :: StateTree stateType w1 c1 e1 cs))
        (Bin _ _ oldChild)
        (Bin (ctor :: Gtk.ManagedPtr w2 -> w2) newAttributes newChild) =
    case (st, eqT @w1 @w2) of
      (StateTreeBin top oldChildState, Just Refl) ->
        Modify $ do
          binWidget <- Gtk.unsafeCastTo ctor (stateTreeWidget top)
          let oldCollected = stateTreeCollectedAttributes top
              newCollected = collectAttributes newAttributes
          updateProperties binWidget (collectedProperties oldCollected) (collectedProperties newCollected)
          updateClasses (stateTreeStyleContext top) (collectedClasses oldCollected) (collectedClasses newCollected)

          let top' = top { stateTreeCollectedAttributes = newCollected }
          case patch oldChildState oldChild newChild of
            Modify modify -> SomeState . StateTreeBin top' <$> modify
            Replace createNew -> do
              Gtk.widgetDestroy =<< someStateWidget oldChildState
              newChildState <- createNew
              childWidget <- someStateWidget newChildState
              Gtk.widgetShow childWidget
              Gtk.containerAdd binWidget childWidget
              return (SomeState (StateTreeBin top' newChildState))
            Keep -> return (SomeState st)
      _ -> Replace (create (Bin ctor newAttributes newChild))

--
-- EventSource
--

instance (BinChild parent child, EventSource child) =>
         EventSource (Bin parent child) where
  subscribe (Bin ctor props child) (SomeState st) cb =
    case st of
      StateTreeBin top childState -> do
        binWidget <- Gtk.unsafeCastTo ctor (stateTreeWidget top)
        handlers' <- foldMap (addSignalHandler cb binWidget) props
        (<> handlers') <$> subscribe child childState cb
      _ -> error "Cannot subscribe to Bin events with a non-bin state tree."

--
-- FromWidget
--

instance ( BinChild widget child
         , Typeable widget
         , Patchable child
         , EventSource child
         , Functor (Bin widget child)
         ) =>
         FromWidget (Bin widget child) event (Widget event) where
  fromWidget = Widget

instance a ~ () => FromWidget (Bin widget child) event (MarkupOf (Bin widget child) event a) where
  fromWidget = single

instance ( BinChild widget child
         , a ~ ()
         , Typeable widget
         , Patchable child
         , EventSource child
         , Functor (Bin widget child)
         ) =>
         FromWidget (Bin widget child) event (Markup event a) where
  fromWidget = single . Widget

instance FromWidget (Bin widget child) event (Bin widget child event) where
  fromWidget = id
