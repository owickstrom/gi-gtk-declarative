{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFunctor          #-}
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

-- | Implementations for common "Gtk.Container".

module GI.Gtk.Declarative.Container
  ( Container
  , container
  )
where

import           Control.Monad                      (forM_)
import           Data.Maybe
import           Data.Typeable
import qualified GI.Gtk                             as Gtk

import           GI.Gtk.Declarative.Attributes
import           GI.Gtk.Declarative.Container.Patch
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Markup
import           GI.Gtk.Declarative.Patch

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
  => (Gtk.ManagedPtr widget -> widget)
  -> [Attribute widget event]
  -> MarkupOf child event ()
  -> target
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
    forM_ (unChildren children) $ \child -> do
      childWidget <- create child
      appendChild widget' child childWidget
    Gtk.toWidget widget'
  patch (Container _ oldAttributes oldChildren) (Container ctor newAttributes newChildren) =
    Modify $ \widget' -> do
      containerWidget <- Gtk.unsafeCastTo ctor widget'
      Gtk.set containerWidget (concatMap extractAttrSetOps newAttributes)
      sc <- Gtk.widgetGetStyleContext widget'
      mapM_ (removeClass sc) oldAttributes
      mapM_ (addClass sc) newAttributes
      patchInContainer
        containerWidget
        (unChildren oldChildren)
        (unChildren newChildren)

--
-- EventSource
--

instance (Typeable child, EventSource child) =>
         EventSource (Container widget (Children child)) where
  subscribe (Container ctor props children) widget' cb = do
    parentWidget <- Gtk.unsafeCastTo ctor widget'
    handlers' <- catMaybes <$> mapM (addSignalHandler cb parentWidget) props
    childWidgets <- Gtk.containerGetChildren parentWidget
    subs <-
      flip foldMap (zip (unChildren children) childWidgets) $ \(c, w) ->
        subscribe c w cb
    return (Subscription handlers' <> subs)

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
