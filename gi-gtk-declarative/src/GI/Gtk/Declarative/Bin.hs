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

-- | A declarative representation of 'Gtk.Bin' in GTK.
module GI.Gtk.Declarative.Bin
  ( Bin
  , bin
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


-- | Supported 'Gtk.Bin's.
class BinChild bin (child :: * -> *) | bin -> child where
  getChild :: bin -> IO Gtk.Widget

instance BinChild Gtk.ScrolledWindow Widget where
  getChild scrolledWindow = do
    viewPort <- getBinChild Gtk.Viewport scrolledWindow
    getBinChild Gtk.Widget viewPort

instance BinChild Gtk.ListBoxRow Widget where
  getChild = getBinChild Gtk.Widget

instance BinChild Gtk.Window Widget where
  getChild = getBinChild Gtk.Widget

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
     , BinChild widget child
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

    Gtk.containerAdd widget' =<< create child
    Gtk.toWidget widget'

  patch (Bin _ oldAttributes oldChild) (Bin ctor newAttributes newChild) =
    Modify $ \widget' -> do

      binWidget <- Gtk.unsafeCastTo ctor widget'
      Gtk.set binWidget (concatMap extractAttrSetOps newAttributes)

      sc <- Gtk.widgetGetStyleContext binWidget
      mapM_ (removeClass sc) oldAttributes
      mapM_ (addClass sc) newAttributes

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

instance (BinChild parent child, EventSource child) =>
         EventSource (Bin parent child) where
  subscribe (Bin ctor props child) widget' cb = do
    binWidget <- Gtk.unsafeCastTo ctor widget'
    handlers' <-
      mconcat . catMaybes <$> mapM (addSignalHandler cb binWidget) props
    childWidget <- getChild binWidget
    (<> handlers') <$> subscribe child childWidget cb

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
