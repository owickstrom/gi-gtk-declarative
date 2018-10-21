{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

-- | A declarative representation of 'Gtk.Bin' in GTK.
module GI.Gtk.Declarative.Bin
  ( Bin (..)
  , bin
  , BinChild
  )
where

import           Control.Monad                          ((>=>))
import           Data.Maybe
import           Data.Typeable
import qualified GI.GObject                             as GI
import qualified GI.Gtk                                 as Gtk

import           GI.Gtk.Declarative.Attributes
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
    -> [Attribute widget event]
    -> child event
    -> Bin widget child event

instance Functor (Bin widget child) where
  fmap f (Bin ctor attrs child) =
    Bin ctor (fmap f <$> attrs) (fmap f child)

-- | Construct a /bin/ widget, i.e. a widget with exactly one child.
bin ::
     ( Patchable (Bin widget child)
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
  -> [Attribute widget event]          -- ^ List of 'Attribute's.
  -> child event                       -- ^ The bin's child widget, whose type is decided by the 'BinChild' instance.
  -> target                            -- ^ The target, whose type is decided by 'FromWidget'.
bin ctor attrs = fromWidget . Bin ctor attrs

--
-- Patchable
--

instance (BinChild parent child, Patchable child) => Patchable (Bin parent child) where
  create (Bin ctor props child) = do
    let attrOps = concatMap extractAttrConstructOps props
    widget' <- Gtk.new ctor attrOps

    sc <- Gtk.widgetGetStyleContext widget'
    mapM_ (addClass sc) props
    mapM_ (applyAfterCreated widget') props
    childState <- create child
    Gtk.containerAdd widget' (stateTreeWidget (stateTreeNode childState))
    w <- Gtk.toWidget widget'
    return (StateTreeBin (StateTreeNode w sc) childState)

  patch (StateTreeBin top oldChildState) (Bin _ oldAttributes oldChild) (Bin ctor newAttributes newChild) =
    Modify $ do

      binWidget <- Gtk.unsafeCastTo ctor (stateTreeWidget top)
      Gtk.set binWidget (concatMap extractAttrSetOps newAttributes)
      let sc = stateTreeStyleContext top
      mapM_ (removeClass sc) oldAttributes
      mapM_ (addClass sc) newAttributes

      case patch oldChildState oldChild newChild of
        Modify modify -> StateTreeBin top <$> modify
        Replace createNew -> do
          Gtk.containerRemove binWidget (stateTreeNodeWidget oldChildState)
          newChildState <- createNew
          Gtk.containerAdd binWidget (stateTreeNodeWidget newChildState)
          return (StateTreeBin top newChildState)
        Keep -> return (StateTreeBin top oldChildState)
  patch _ _ new = Replace (create new)

--
-- EventSource
--

instance (BinChild parent child, EventSource child) =>
         EventSource (Bin parent child) where
  subscribe (Bin ctor props child) (StateTreeBin top childState) cb = do
    binWidget <- Gtk.unsafeCastTo ctor (stateTreeWidget top)
    handlers' <-
      mconcat . catMaybes <$> mapM (addSignalHandler cb binWidget) props
    (<> handlers') <$> subscribe child childState cb

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
