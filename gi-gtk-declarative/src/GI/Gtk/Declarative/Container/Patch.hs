{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors -fno-warn-orphans #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | This module implements the patch algorithm for containers.
module GI.Gtk.Declarative.Container.Patch
  ( IsContainer(..)
  , patchInContainer
  )
where

import           Data.Coerce                        (coerce)
import           Data.Foldable                      (foldMap)
import           Data.Vector                        (Vector, (!?))
import qualified Data.Vector                        as Vector
import           GHC.Ptr                            (nullPtr)
import qualified GI.GLib                            as GLib
import qualified GI.Gtk                             as Gtk

import           GI.Gtk.Declarative.Bin
import           GI.Gtk.Declarative.Container.Box
import           GI.Gtk.Declarative.Container.Class
import           GI.Gtk.Declarative.Container.Paned
import           GI.Gtk.Declarative.Markup
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.State

-- | Patch all children in a container. This does not feature any ID checking,
-- as seen in React, so reordering children in a container can produce many
-- updates.
patchInContainer
  :: ( Gtk.IsWidget container
     , Gtk.IsContainer container
     , Patchable child
     , IsContainer container child
     )
  => StateTree 'ContainerState container child event cs
  -> container
  -> Vector (child e1)
  -> Vector (child e2)
  -> IO (StateTree 'ContainerState container child event cs)
patchInContainer (StateTreeContainer top children) container os' ns' = do
  let maxLength = maximum ([length children, length os', length ns'] :: [Int])
      indices   = Vector.enumFromN 0 (fromIntegral maxLength)
  newChildren <- foldMap
    go
    (Vector.zip4 indices
                 (padMaybes maxLength children)
                 (padMaybes maxLength os')
                 (padMaybes maxLength ns')
    )

  Gtk.widgetQueueResize container
  return (StateTreeContainer top newChildren)
  where
    go = \case
      -- In case we have a corresponding old and new declarative widget, we patch
      -- the GTK widget.
      (i, Just oldChildState, Just old, Just new) ->
        case patch oldChildState old new of
          Modify  modify       -> pure <$> modify
          Replace createWidget -> do
            newChildState  <- createWidget
            oldChildWidget <- someStateWidget oldChildState
            newChildWidget <- someStateWidget newChildState
            replaceChild container new i oldChildWidget newChildWidget
            return (pure newChildState)
          Keep -> return (pure oldChildState)

      -- When there is a new declarative widget, but there already exists a GTK
      -- widget in the corresponding place, we need to replace the GTK widget with
      -- one created from the declarative widget.
      (i, Just oldChildState, Nothing, Just new) -> do
        newChildState  <- create new
        oldChildWidget <- someStateWidget oldChildState
        newChildWidget <- someStateWidget newChildState
        replaceChild container new i oldChildWidget newChildWidget
        return (Vector.singleton newChildState)

      -- When there is a new declarative widget, or one that lacks a corresponding
      -- GTK widget, create and add it.
      (_i, Nothing, _, Just n) -> do
        newChildState <- create n
        w             <- someStateWidget newChildState
        appendChild container n w
        return (Vector.singleton newChildState)

      -- When a declarative widget has been removed, remove the GTK widget from
      -- the container.
      (_i, Just childState, Just _, Nothing) -> do
        Gtk.widgetDestroy =<< someStateWidget childState
        return Vector.empty

      -- When there are more old declarative widgets than GTK widgets, we can
      -- safely ignore the old declarative widgets.
      (_i, Nothing        , Just _ , Nothing) -> return Vector.empty

      -- But, when there are stray GTK widgets without corresponding
      -- declarative widgets, something has gone wrong, and we clean that up by
      -- removing the GTK widgets.
      (_i, Just childState, Nothing, Nothing) -> do
        Gtk.widgetDestroy =<< someStateWidget childState
        return Vector.empty

      -- No more GTK widgets or declarative widgets, we are done.
      (_i, Nothing, Nothing, Nothing) -> return Vector.empty

padMaybes :: Int -> Vector a -> Vector (Maybe a)
padMaybes len xs = Vector.generate len (xs !?)

instance IsContainer Gtk.ListBox (Bin Gtk.ListBoxRow Widget) where
  appendChild box _ widget' =
    Gtk.listBoxInsert box widget' (-1)
  replaceChild box _ i old new = do
    Gtk.widgetDestroy old
    Gtk.listBoxInsert box new i

instance IsContainer Gtk.Box BoxChild where
  appendChild box BoxChild {expand, fill, padding} widget' =
    Gtk.boxPackStart box widget' expand fill padding
  replaceChild box boxChild' i old new = do
    Gtk.widgetDestroy old
    appendChild box boxChild' new
    Gtk.boxReorderChild box new i

-- TODO: Rewrite this as a custom widget, with a type only permitting
-- 2 child widgets. Warning and ignoring widgets is not great.
instance IsContainer Gtk.Paned Pane where
  appendChild paned Pane{resize, shrink} widget' = do
    c1 <- Gtk.panedGetChild1 paned
    c2 <- Gtk.panedGetChild2 paned
    case (c1, c2) of
      (Nothing, Nothing) -> Gtk.panedPack1 paned widget' (coerce resize) (coerce shrink)
      (Just _, Nothing) -> Gtk.panedPack2 paned widget' (coerce resize) (coerce shrink)
      _ -> GLib.logDefaultHandler
           (Just "gi-gtk-declarative")
           [GLib.LogLevelFlagsLevelWarning]
           (Just "appendChild: The `GI.Gtk.Paned` widget can only fit 2 panes. Additional children will be ignored.")
           nullPtr
  replaceChild paned Pane{resize, shrink} i old new = do
    Gtk.widgetDestroy old
    case i of
      0 -> Gtk.panedPack1 paned new (coerce resize) (coerce shrink)
      1 -> Gtk.panedPack2 paned new (coerce resize) (coerce shrink)
      _ -> GLib.logDefaultHandler
           (Just "gi-gtk-declarative")
           [GLib.LogLevelFlagsLevelWarning]
           (Just "replaceChild: The `GI.Gtk.Paned` widget can only fit 2 panes. Additional children will be ignored.")
           nullPtr
