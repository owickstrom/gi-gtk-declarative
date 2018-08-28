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

import           GI.Gtk.Declarative.Container.Patch
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Markup
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.Props

data Container widget children event where
  Container
    :: (Typeable widget, Gtk.IsWidget widget, Gtk.IsContainer widget)
    => (Gtk.ManagedPtr widget -> widget)
    -> [PropPair widget event]
    -> children
    -> Container widget children event

container
  :: ( Patchable (Container widget (MarkupOf child event ()))
     , Typeable widget
     , Typeable child
     , Typeable event
     , Gtk.IsWidget widget
     , Gtk.IsContainer widget
     , FromWidget (Container widget (MarkupOf child event ())) event target
     )
  => (Gtk.ManagedPtr widget -> widget)
  -> [PropPair widget event]
  -> MarkupOf child event ()
  -> target
container ctor attrs = fromWidget . Container ctor attrs

--
-- Patchable
--

instance (Typeable event, Patchable child, IsContainer container child event)
  => Patchable (Container container (MarkupOf child event ())) where
  create (Container ctor props children) = do
    let attrOps = concatMap extractAttrConstructOps props
    widget' <- Gtk.new ctor attrOps
    sc <- Gtk.widgetGetStyleContext widget'
    mapM_ (addClass sc) props
    forM_ (runMarkup children) $ \child -> do
      childWidget <- create child
      appendChild widget' child childWidget
    Gtk.toWidget widget'
  patch (Container _ oldProps oldChildren) (Container ctor newProps newChildren) =
    Modify $ \widget' -> do
      containerWidget <- Gtk.unsafeCastTo ctor widget'
      Gtk.set containerWidget (concatMap extractAttrSetOps newProps)
      sc <- Gtk.widgetGetStyleContext widget'
      mapM_ (removeClass sc) oldProps
      mapM_ (addClass sc) newProps
      patchInContainer containerWidget (runMarkup oldChildren) (runMarkup newChildren)

--
-- EventSource
--

instance (Typeable child, Typeable event, EventSource (child event) event)
  => EventSource (Container widget (MarkupOf child event ()) event) event where
  subscribe (Container ctor props children) widget' cb = do
    parentWidget <- Gtk.unsafeCastTo ctor widget'
    handlers' <- catMaybes <$> mapM (addSignalHandler cb parentWidget) props
    childWidgets <- Gtk.containerGetChildren parentWidget
    subs <- flip foldMap (zip (runMarkup children) childWidgets) $ \(c, w) -> subscribe c w cb
    return (Subscription handlers' <> subs)

--
-- FromWidget
--

instance ( Typeable widget
         , Typeable children
         , Patchable (Container widget children)
         , EventSource (Container widget children event) event
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
         , EventSource (Container widget children event) event
         ) =>
         FromWidget (Container widget children) event (Markup event a) where
  fromWidget = single . Widget
