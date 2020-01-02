{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors -fno-warn-orphans #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | Implementation of 'Gtk.Paned' as a declarative container.
module GI.Gtk.Declarative.Container.Paned
  ( Pane
  , PaneProperties(..)
  , defaultPaneProperties
  , pane
  , paned
  )
where

import           Data.Coerce                    ( coerce )
import           Data.Default.Class             ( Default(def) )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as Vector
import           GHC.Ptr                        ( nullPtr )
import qualified GI.GLib                       as GLib
import qualified GI.Gtk                        as Gtk

import           GI.Gtk.Declarative.Attributes
import           GI.Gtk.Declarative.Container
import           GI.Gtk.Declarative.Container.Class
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.Widget

-- | Describes a pane to be packed with
-- 'Gtk.panePack1'/'Gtk.panePack2' in a 'Gtk.Paned'.
data Pane event = Pane
  { paneProperties :: PaneProperties
  , paneChild      :: Widget event
  }
  deriving (Functor)

-- | Values used when packing a pane into a 'Gtk.Paned'.
data PaneProperties = PaneProperties
  { resize :: Bool
  , shrink :: Bool
  }

-- | Defaults for 'PaneProperties'. Use these and override specific
-- fields.
defaultPaneProperties :: PaneProperties
defaultPaneProperties = PaneProperties { resize = False, shrink = True }

instance Default PaneProperties where
  def = defaultPaneProperties

-- | Construct a pane to be packed with
-- 'Gtk.panePack1'/'Gtk.panePack2' in a 'Gtk.Paned'.
pane :: PaneProperties -> Widget event -> Pane event
pane paneProperties paneChild = Pane { .. }

instance Patchable Pane where
  create = create . paneChild
  patch s b1 b2 = patch s (paneChild b1) (paneChild b2)

instance EventSource Pane where
  subscribe Pane {..} = subscribe paneChild

-- | Construct a 'Gtk.Paned' based on attributes and two child 'Pane's.
paned
  :: Vector (Attribute Gtk.Paned event)
  -> Pane event
  -> Pane event
  -> Widget event
paned attrs p1 p2 = container Gtk.Paned attrs (Panes p1 p2)

data Panes child = Panes child child
  deriving (Functor)

instance IsContainer Gtk.Paned Pane where
  appendChild paned' Pane { paneProperties = PaneProperties { resize, shrink } } widget'
    = do
      c1 <- Gtk.panedGetChild1 paned'
      c2 <- Gtk.panedGetChild2 paned'
      case (c1, c2) of
        (Nothing, Nothing) ->
          Gtk.panedPack1 paned' widget' (coerce resize) (coerce shrink)
        (Just _, Nothing) ->
          Gtk.panedPack2 paned' widget' (coerce resize) (coerce shrink)
        _ -> GLib.logDefaultHandler
          (Just "gi-gtk-declarative")
          [GLib.LogLevelFlagsLevelWarning]
          (Just
            "appendChild: The `GI.Gtk.Paned` widget can only fit 2 panes. Additional children will be ignored."
          )
          nullPtr
  replaceChild paned' Pane { paneProperties = PaneProperties { resize, shrink } } i old new
    = do
      Gtk.widgetDestroy old
      case i of
        0 -> Gtk.panedPack1 paned' new (coerce resize) (coerce shrink)
        1 -> Gtk.panedPack2 paned' new (coerce resize) (coerce shrink)
        _ -> GLib.logDefaultHandler
          (Just "gi-gtk-declarative")
          [GLib.LogLevelFlagsLevelWarning]
          (Just
            "replaceChild: The `GI.Gtk.Paned` widget can only fit 2 panes. Additional children will be ignored."
          )
          nullPtr

instance ToChildren Gtk.Paned Panes Pane where
  toChildren _ (Panes p1 p2) = Children (Vector.fromList [p1, p2])
