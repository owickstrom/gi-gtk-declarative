{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE RecordWildCards        #-}

-- | Implementations of 'Patchable' for common GTK+ container widgets.

module GI.Gtk.Declarative.Container
  ( PatchableContainer(..)
  , ContainerEventSource(..)
  , container
  )
where

import           Control.Monad.IO.Class                   ( MonadIO )
import           Data.Maybe
import qualified Data.GI.Base                  as GI
import qualified Data.GI.Base.Attributes       as GI
import           Data.Typeable
import qualified GI.Gtk                        as Gtk

import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Markup
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.Props

class PatchableContainer widget children where
  createChildrenIn :: widget -> children -> IO ()
  patchChildrenIn :: widget -> children -> children -> IO ()

class ContainerEventSource widget children event | children -> event where
  subscribeChildren :: children -> widget -> (event -> IO ()) -> IO Subscription

-- * ScrolledWindow

requireSingle :: String -> [w] -> IO w
requireSingle what = \case
  [w] -> return w
  _   -> fail ("Expected a single " ++ what ++ " in the container.")

instance Typeable event => PatchableContainer Gtk.ScrolledWindow (Widget event) where
  createChildrenIn box child = create child >>= Gtk.containerAdd box
  patchChildrenIn scrolledWindow oldChild newChild = do
    viewport <- Gtk.containerGetChildren scrolledWindow
      >>= requireSingle "Viewport"
      >>= Gtk.unsafeCastTo Gtk.Viewport
    childWidget <- Gtk.containerGetChildren viewport
      >>= requireSingle "scrolled child"
    case patch oldChild newChild of
      Modify modify -> modify childWidget
      Replace createNew -> do
        Gtk.containerRemove viewport childWidget
        Gtk.containerAdd viewport =<< createNew
      Keep -> return ()
    where

instance ContainerEventSource Gtk.ScrolledWindow (Widget event) event where
  subscribeChildren child scrolledWindow cb = do
    viewport <- Gtk.containerGetChildren scrolledWindow
      >>= requireSingle "Viewport"
      >>= Gtk.unsafeCastTo Gtk.Viewport
    childWidget <- Gtk.containerGetChildren viewport
      >>= requireSingle "scrolled child"
    subscribe child childWidget cb

-- * Container object

data GtkContainer widget children event where
  GtkContainer
    :: (Typeable widget, Gtk.IsWidget widget)
    => (Gtk.ManagedPtr widget -> widget)
    -> [PropPair widget event]
    -> children
    -> GtkContainer widget children event

instance Show (GtkContainer widget children a) where
  show = \case
    GtkContainer{} -> "GtkContainer"

extractAttrConstructOps
  :: PropPair widget event -> [GI.AttrOp widget 'GI.AttrConstruct]
extractAttrConstructOps = \case
  (attr := value) -> pure (attr Gtk.:= value)
  _               -> mempty

extractAttrSetOps :: PropPair widget event -> [GI.AttrOp widget 'GI.AttrSet]
extractAttrSetOps = \case
  (attr := value) -> pure (attr Gtk.:= value)
  _               -> mempty

addClass :: MonadIO m => Gtk.StyleContext -> PropPair widget event -> m ()
addClass sc = \case
  Classes cs -> mapM_ (Gtk.styleContextAddClass sc) cs
  _          -> pure ()

removeClass :: MonadIO m => Gtk.StyleContext -> PropPair widget event -> m ()
removeClass sc = \case
  Classes cs -> mapM_ (Gtk.styleContextRemoveClass sc) cs
  _          -> pure ()

addSignalHandler
  :: (Gtk.IsWidget widget, MonadIO m)
  => (event -> IO ())
  -> widget
  -> PropPair widget event
  -> m (Maybe ConnectedHandler)
addSignalHandler onEvent widget' = \case
  OnSignalPure signal handler -> do
    handlerId <- Gtk.on widget' signal (toGtkCallback handler onEvent)
    w         <- Gtk.toWidget widget'
    pure (Just (ConnectedHandler w handlerId))
  _ -> pure Nothing


instance (PatchableContainer widget children) => Patchable (GtkContainer widget children) where
  create (GtkContainer ctor props children) = do
    let attrOps = concatMap extractAttrConstructOps props
    widget' <- Gtk.new ctor attrOps

    sc <- Gtk.widgetGetStyleContext widget'
    mapM_ (addClass sc) props

    createChildrenIn widget' children
    Gtk.toWidget widget'
  patch (GtkContainer _ oldProps oldChildren) (GtkContainer ctor newProps newChildren) =
    Modify $ \widget' -> do

      w <- Gtk.unsafeCastTo ctor widget'
      Gtk.set w (concatMap extractAttrSetOps newProps)

      sc <- Gtk.widgetGetStyleContext widget'
      mapM_ (removeClass sc) oldProps
      mapM_ (addClass sc) newProps

      patchChildrenIn w oldChildren newChildren

instance ContainerEventSource widget children event
  => EventSource (GtkContainer widget children event) event where
  subscribe (GtkContainer ctor props children) widget' cb = do
    w <- Gtk.unsafeCastTo ctor widget'
    handlers' <- catMaybes <$> mapM (addSignalHandler cb w) props
    (<> Subscription handlers') <$> subscribeChildren children w cb

container
  :: ( PatchableContainer widget children
     , ContainerEventSource widget children event
     , Patchable (GtkContainer widget children)
     , Typeable widget
     , Typeable children
     , Typeable event
     , Gtk.IsWidget widget
     , FromWidget markup event
     )
  => (Gtk.ManagedPtr widget -> widget)
  -> [PropPair widget event]
  -> children
  -> markup
container ctor attrs = fromWidget . Widget . GtkContainer ctor attrs
