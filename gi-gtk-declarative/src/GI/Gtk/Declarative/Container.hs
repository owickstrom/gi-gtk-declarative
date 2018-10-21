{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

-- | Implementations for common "Gtk.Container".

module GI.Gtk.Declarative.Container
  ( Container
  , container
  , Children
  )
where

import           Control.Monad                      (forM)
import           Data.Maybe
import           Data.Typeable
import qualified GI.Gtk                             as Gtk

import           GI.Gtk.Declarative.Attributes
import           GI.Gtk.Declarative.Attributes.Internal
import           GI.Gtk.Declarative.Container.Patch
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Markup
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.State

-- | Declarative version of a /container/ widget, i.e. a widget with zero
-- or more child widgets. The type of 'children' is parameterized, and differs
-- across the supported container widgets, as some containers require specific
-- types of child widgets. These type relations are decided by 'IsContainer',
-- and instances can found in "GI.Gtk.Declarative.Container.Patch".
data Container widget children event where
  Container
    :: ( Typeable widget
       , Gtk.IsWidget widget
       , Gtk.IsContainer widget
       , Functor children
       )
    => (Gtk.ManagedPtr widget -> widget)
    -> [Attribute widget event]
    -> children event
    -> Container widget children event

instance Functor (Container widget children) where
  fmap f (Container ctor attrs children) =
    Container ctor (fmap f <$> attrs) (fmap f children)

-- | Construct a /container/ widget, i.e. a widget with zero or more children.
container
  :: ( Patchable (Container widget (Children child))
     , Typeable widget
     , Typeable child
     , Typeable event
     , Functor child
     , Gtk.IsWidget widget
     , Gtk.IsContainer widget
     , FromWidget (Container widget (Children child)) event target
     )
  => (Gtk.ManagedPtr widget -> widget) -- ^ A container widget constructor from the underlying gi-gtk library.
  -> [Attribute widget event]          -- ^ List of 'Attribute's.
  -> MarkupOf child event ()           -- ^ The container's 'child' widgets, in a 'MarkupOf' builder.
  -> target                            -- ^ The target, whose type is decided by 'FromWidget'.
container ctor attrs = fromWidget . Container ctor attrs . toChildren

newtype Children child event = Children { unChildren :: [child event] }
  deriving (Functor)

toChildren :: MarkupOf child event () -> Children child event
toChildren = Children . runMarkup

--
-- Patchable
--

instance (Patchable child, IsContainer container child) =>
         Patchable (Container container (Children child)) where
  create (Container ctor props children) = do
    let attrOps = concatMap extractAttrConstructOps props
    widget' <- Gtk.new ctor attrOps
    sc <- Gtk.widgetGetStyleContext widget'
    mapM_ (addClass sc) props
    childStates <-
      forM (unChildren children) $ \child -> do
        childState <- create child
        appendChild widget' child (stateTreeNodeWidget childState)
        return childState
    mapM_ (applyAfterCreated widget') props
    w <- Gtk.toWidget widget'
    return (StateTreeContainer (StateTreeNode w sc) childStates)
  patch (StateTreeContainer top childStates) (Container _ oldAttributes oldChildren) (Container ctor newAttributes newChildren) =
    Modify $ do
      containerWidget <- Gtk.unsafeCastTo ctor (stateTreeWidget top)
      Gtk.set containerWidget (concatMap extractAttrSetOps newAttributes)
      let sc = stateTreeStyleContext top
      mapM_ (removeClass sc) oldAttributes
      mapM_ (addClass sc) newAttributes
      patchInContainer
        (StateTreeContainer top childStates)
        containerWidget
        (unChildren oldChildren)
        (unChildren newChildren)
  patch _ _ new = Replace (create new)

--
-- EventSource
--

instance (Typeable child, EventSource child) =>
         EventSource (Container widget (Children child)) where
  subscribe (Container ctor props children) (StateTreeContainer top childStates) cb = do
    parentWidget <- Gtk.unsafeCastTo ctor (stateTreeWidget top)
    handlers' <- mconcat . catMaybes <$> mapM (addSignalHandler cb parentWidget) props
    childWidgets <- Gtk.containerGetChildren parentWidget
    subs <-
      flip foldMap (zip (unChildren children) childStates) $ \(c, childState) ->
        subscribe c childState cb
    return (handlers' <> subs)

--
-- FromWidget
--

instance ( Typeable widget
         , Typeable children
         , Patchable (Container widget children)
         , EventSource (Container widget children)
         , Functor (Container widget children)
         ) =>
         FromWidget (Container widget children) event (Widget event) where
  fromWidget = Widget

instance a ~ () =>
         FromWidget (Container widget children) event (MarkupOf (Container widget children) event a) where
  fromWidget = single

instance ( a ~ ()
         , Typeable widget
         , Typeable children
         , Patchable (Container widget children)
         , EventSource (Container widget children)
         , Functor (Container widget children)
         ) =>
         FromWidget (Container widget children) event (Markup event a) where
  fromWidget = single . Widget

instance FromWidget (Container widget children) event (Container widget children event) where
  fromWidget = id
