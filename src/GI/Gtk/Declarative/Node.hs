{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

-- | A 'Node' represents a declarative "leaf" widget, i.e. one that is
-- not a container with children.

module GI.Gtk.Declarative.Node
  ( Node
  , node
  )
where

import           Control.Monad                            ( void )
import           Control.Monad.IO.Class                   ( MonadIO )
import qualified Data.GI.Base.Attributes       as GI
import           Data.Typeable
import qualified GI.Gtk                        as Gtk

import           GI.Gtk.Declarative.Markup
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.Props

data Node event where
  Node
    :: (Typeable w, Gtk.IsWidget w)
    => (Gtk.ManagedPtr w -> w)
    -> [PropPair w]
    -> Node event

instance Functor Node where
  fmap _f (Node ctor props) = (Node ctor props)

extractAttrConstructOps :: PropPair widget -> [GI.AttrOp widget 'GI.AttrConstruct]
extractAttrConstructOps = \case
  (attr := value) -> pure (attr Gtk.:= value)
  _               -> []

extractAttrSetOps :: PropPair widget -> [GI.AttrOp widget 'GI.AttrSet]
extractAttrSetOps = \case
  (attr := value) -> pure (attr Gtk.:= value)
  _               -> []

addClass :: MonadIO m => Gtk.StyleContext -> PropPair widget -> m ()
addClass sc = \case
  Classes cs -> mapM_ (Gtk.styleContextAddClass sc) cs
  _          -> pure ()

removeClass :: MonadIO m => Gtk.StyleContext -> PropPair widget -> m ()
removeClass sc = \case
  Classes cs -> mapM_ (Gtk.styleContextRemoveClass sc) cs
  _          -> pure ()

addSignalHandler :: MonadIO m => widget -> PropPair widget -> m ()
addSignalHandler widget = \case
  OnSignal signal handler -> void (Gtk.on widget signal (handler widget))
  _                       -> pure ()

instance Patchable Node where
  create = \case
    (Node ctor props) -> do
        let attrOps = concatMap extractAttrConstructOps props
        widget <- Gtk.new ctor attrOps

        sc <- Gtk.widgetGetStyleContext widget
        mapM_ (addClass sc) props

        mapM_ (addSignalHandler widget) props

        Gtk.widgetShowAll widget
        Gtk.toWidget widget
  patch (Node _ oldProps) (Node ctor newProps) = Modify $ \widget -> do
    w <- Gtk.unsafeCastTo ctor widget
    Gtk.set w (concatMap extractAttrSetOps newProps)

    sc <- Gtk.widgetGetStyleContext widget
    mapM_ (removeClass sc) oldProps
    mapM_ (addClass sc) newProps

    Gtk.widgetShowAll w

node
  :: (Typeable widget, Typeable e, Gtk.IsWidget widget)
  => (Gtk.ManagedPtr widget -> widget)
  -> [PropPair widget]
  -> Markup e
node ctor attrs = Markup (Node ctor attrs)
