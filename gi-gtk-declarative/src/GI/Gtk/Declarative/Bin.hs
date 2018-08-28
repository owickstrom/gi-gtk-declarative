{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Implementations for common "Gtk.Bin"s.

module GI.Gtk.Declarative.Bin
  ( Bin
  , bin
  )
where

import           Control.Monad                  ((>=>))
import           Data.Maybe
import           Data.Typeable
import qualified GI.GObject                     as GI
import qualified GI.Gtk                         as Gtk

import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Markup
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.Props


-- | Supported "Gtk.Bin"s.
class BinChild bin (child :: * -> *) | bin -> child where
  getChild :: bin -> IO Gtk.Widget

instance BinChild Gtk.ScrolledWindow Widget where
  getChild scrolledWindow = do
    viewPort <- getBinChild Gtk.Viewport scrolledWindow
    getBinChild Gtk.Widget viewPort

instance BinChild Gtk.ListBoxRow Widget where
  getChild = getBinChild Gtk.Widget

--
-- Bin
--

data Bin widget child event where
  Bin
    :: (Typeable widget, Gtk.IsContainer widget, Gtk.IsBin widget, Gtk.IsWidget widget)
    => (Gtk.ManagedPtr widget -> widget)
    -> [PropPair widget event]
    -> child event
    -> Bin widget child event

bin
  :: ( Patchable (Bin widget child)
     , Typeable widget
     , Typeable child
     , Typeable event
     , Gtk.IsContainer widget
     , Gtk.IsBin widget
     , Gtk.IsWidget widget
     , FromWidget (Bin widget child) event target
     , BinChild widget child
     )
  => (Gtk.ManagedPtr widget -> widget)
  -> [PropPair widget event]
  -> child event
  -> target
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

    Gtk.containerAdd widget' =<< create child
    Gtk.toWidget widget'

  patch (Bin _ oldProps oldChild) (Bin ctor newProps newChild) =
    Modify $ \widget' -> do

      binWidget <- Gtk.unsafeCastTo ctor widget'
      Gtk.set binWidget (concatMap extractAttrSetOps newProps)

      sc <- Gtk.widgetGetStyleContext binWidget
      mapM_ (removeClass sc) oldProps
      mapM_ (addClass sc) newProps

      childWidget <- getChild binWidget

      case patch oldChild newChild of
        Modify modify -> modify childWidget
        Replace createNew -> do
          Gtk.containerRemove binWidget childWidget
          Gtk.containerAdd binWidget =<< createNew
        Keep -> return ()

--
-- EventSource
--

instance (BinChild parent child, EventSource (child event) event) => EventSource (Bin parent child event) event where
  subscribe (Bin ctor props child) widget' cb = do
    binWidget <- Gtk.unsafeCastTo ctor widget'
    handlers' <- catMaybes <$> mapM (addSignalHandler cb binWidget) props
    childWidget <- getChild binWidget
    (<> Subscription handlers') <$> subscribe child childWidget cb

--
-- FromWidget
--

instance (BinChild widget child, Typeable widget, Patchable child, EventSource (child event) event)
  => FromWidget (Bin widget child) event (Widget event) where
  fromWidget = Widget

instance a ~ () => FromWidget (Bin widget child) event (MarkupOf (Bin widget child) event a) where
  fromWidget = widget

instance (BinChild widget child, a ~ (), Typeable widget, Patchable child, EventSource (child event) event)
  => FromWidget (Bin widget child) event (Markup event a) where
  fromWidget = widget . Widget


-- | Get a "Gtk.Bin" child, or fail, and cast it to the given widget type.
getBinChild
  :: (Gtk.IsBin bin, GI.GObject child)
  => (Gtk.ManagedPtr child -> child)
  -> bin
  -> IO child
getBinChild ctor =
  Gtk.binGetChild
    >=> maybe (fail "expected Bin to have a child") return
    >=> Gtk.unsafeCastTo ctor

