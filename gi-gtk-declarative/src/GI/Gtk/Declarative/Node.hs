{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}

-- | A 'Node' represents a declarative "leaf" widget, i.e. one that is
-- not a container with children.

module GI.Gtk.Declarative.Node
  ( Node
  , node
  )
where

import           Data.Maybe
import           Data.Typeable
import qualified GI.Gtk                         as Gtk

import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Markup
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.Props

data Node widget event where
  Node
    :: (Typeable widget, Gtk.IsWidget widget)
    => (Gtk.ManagedPtr widget -> widget)
    -> [PropPair widget event]
    -> Node widget event

instance Patchable (Node widget) where
  create = \case
    (Node ctor props) -> do
        let attrOps = concatMap extractAttrConstructOps props
        widget' <- Gtk.new ctor attrOps

        sc <- Gtk.widgetGetStyleContext widget'
        mapM_ (addClass sc) props

        Gtk.widgetShowAll widget'
        Gtk.toWidget widget'
  patch (Node (_ :: Gtk.ManagedPtr w1 -> w1) oldProps) (Node (ctor :: Gtk.ManagedPtr w2 -> w2) newProps) =
    case eqT @w1 @w2 of
      Just Refl ->
        Modify $ \widget' -> do
          w <- Gtk.unsafeCastTo ctor widget'
          Gtk.set w (concatMap extractAttrSetOps newProps)

          sc <- Gtk.widgetGetStyleContext widget'
          mapM_ (removeClass sc) oldProps
          mapM_ (addClass sc) newProps

          Gtk.widgetShowAll w

      Nothing -> Replace (create (Node ctor newProps))

instance EventSource (Node widget event) event where
  subscribe (Node ctor props) widget' cb = do
    w <- Gtk.unsafeCastTo ctor widget'
    Subscription . catMaybes <$> mapM (addSignalHandler cb w) props

instance Typeable widget => FromWidget (Node widget) event (Widget event) where
  fromWidget = Widget

instance FromWidget (Node widget) event (MarkupOf (Node widget) event ()) where
  fromWidget = widget

instance Typeable widget => FromWidget (Node widget) event (Markup event ()) where
  fromWidget = widget . Widget

node ::
     ( Typeable widget
     , Typeable event
     , Gtk.IsWidget widget
     , FromWidget (Node widget) event target
     )
  => (Gtk.ManagedPtr widget -> widget)
  -> [PropPair widget event]
  -> target
node ctor = fromWidget . Node ctor

