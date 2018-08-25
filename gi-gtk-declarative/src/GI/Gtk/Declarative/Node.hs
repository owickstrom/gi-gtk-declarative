{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeOperators          #-}

-- | A 'Node' represents a declarative "leaf" widget, i.e. one that is
-- not a container with children.

module GI.Gtk.Declarative.Node
  ( Node
  , node
  )
where

import           Control.Monad.IO.Class         (MonadIO)
import qualified Data.GI.Base.Attributes        as GI
import           Data.Maybe
import           Data.Typeable
import qualified GI.Gtk                         as Gtk

import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Markup
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.Props

data Node event where
  Node
    :: (Typeable widget, Gtk.IsWidget widget)
    => (Gtk.ManagedPtr widget -> widget)
    -> [PropPair widget event]
    -> Node event

instance Functor Node where
  fmap f (Node ctor props) = (Node ctor (fmap (fmap f) props))

extractAttrConstructOps
  :: PropPair widget event -> [GI.AttrOp widget 'GI.AttrConstruct]
extractAttrConstructOps = \case
  (attr := value) -> pure (attr Gtk.:= value)
  _               -> []

extractAttrSetOps :: PropPair widget event -> [GI.AttrOp widget 'GI.AttrSet]
extractAttrSetOps = \case
  (attr := value) -> pure (attr Gtk.:= value)
  _               -> []

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
  OnSignalImpure signal handler -> do
    handlerId <- Gtk.on widget' signal (toGtkCallback handler onEvent widget')
    w         <- Gtk.toWidget widget'
    pure (Just (ConnectedHandler w handlerId))
  _ -> pure Nothing

instance Patchable Node where
  create = \case
    (Node ctor props) -> do
        let attrOps = concatMap extractAttrConstructOps props
        widget' <- Gtk.new ctor attrOps

        sc <- Gtk.widgetGetStyleContext widget'
        mapM_ (addClass sc) props

        Gtk.widgetShowAll widget'
        Gtk.toWidget widget'
  patch (Node _ oldProps) (Node ctor newProps) = Modify $ \widget' -> do
    w <- Gtk.unsafeCastTo ctor widget'
    Gtk.set w (concatMap extractAttrSetOps newProps)

    sc <- Gtk.widgetGetStyleContext widget'
    mapM_ (removeClass sc) oldProps
    mapM_ (addClass sc) newProps

    Gtk.widgetShowAll w

instance EventSource (Node event) event where
  subscribe (Node ctor props) widget' cb = do
    w <- Gtk.unsafeCastTo ctor widget'
    Subscription . catMaybes <$> mapM (addSignalHandler cb w) props

node ::
     ( Typeable widget
     , Typeable event
     , Gtk.IsWidget widget
     , FromWidget markup event
     )
  => (Gtk.ManagedPtr widget -> widget)
  -> [PropPair widget event]
  -> markup
node ctor = fromWidget . Widget . Node ctor
