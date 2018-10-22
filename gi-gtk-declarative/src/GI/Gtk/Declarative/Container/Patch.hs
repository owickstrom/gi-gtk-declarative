{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors -fno-warn-orphans #-}

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

-- | This module implements the patch algorithm for containers.
module GI.Gtk.Declarative.Container.Patch
  ( IsContainer(..)
  , patchInContainer
  )
where

import           Control.Monad                    (forM)
import           Data.List                        (zip4)
import qualified GI.Gtk                           as Gtk

import           GI.Gtk.Declarative.Bin
import           GI.Gtk.Declarative.Container.Box
import           GI.Gtk.Declarative.Container.Class
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
  => StateTree 'ContainerState container child event
  -> container
  -> [child e1]
  -> [child e2]
  -> IO (StateTree 'ContainerState container child event)
patchInContainer (StateTreeContainer top children) container os' ns' = do
  let maxLength = maximum [length children, length os', length ns']
      indices   = [0 .. pred (fromIntegral maxLength)]
  newChildren <-
    forM (zip4 indices (padMaybes children) (padMaybes os') (padMaybes ns'))
      $ \case

          -- In case we have a corresponding old and new declarative widget, we patch
          -- the GTK widget.
          (i, Just oldChildState, Just old, Just new) -> case patch oldChildState old new of
            Modify  modify       -> pure <$> modify
            Replace createWidget -> do
              newChildState <- createWidget
              oldChildWidget <- someStateWidget oldChildState
              newChildWidget <- someStateWidget newChildState
              replaceChild container new i oldChildWidget newChildWidget
              return [newChildState]
            Keep -> return [oldChildState]

          -- When there is a new declarative widget, but there already exists a GTK
          -- widget in the corresponding place, we need to replace the GTK widget with
          -- one created from the declarative widget.
          (i, Just oldChildState, Nothing, Just new) -> do
            newChildState <- create new
            oldChildWidget <- someStateWidget oldChildState
            newChildWidget <- someStateWidget newChildState
            replaceChild container new i oldChildWidget newChildWidget
            return [newChildState]

          -- When there is a new declarative widget, or one that lacks a corresponding
          -- GTK widget, create and add it.
          (_i, Nothing, _, Just n) -> do
            newChildState <- create n
            w <- someStateWidget newChildState
            appendChild container n w
            Gtk.widgetShow w
            return [newChildState]

          -- When a declarative widget has been removed, remove the GTK widget from
          -- the container.
          (_i, Just childState, Just _, Nothing) -> do
            Gtk.widgetDestroy =<< someStateWidget childState
            return []

          -- When there are more old declarative widgets than GTK widgets, we can
          -- safely ignore the old declarative widgets.
          (_i, Nothing   , Just _ , Nothing) -> return []

          -- But, when there are stray GTK widgets without corresponding
          -- declarative widgets, something has gone wrong, and we clean that up by
          -- removing the GTK widgets.
          (_i, Just childState, Nothing, Nothing) -> do
            Gtk.widgetDestroy =<< someStateWidget childState
            return []

          -- No more GTK widgets or declarative widgets, we are done.
          (_i, Nothing, Nothing, Nothing) -> return []

  Gtk.widgetQueueResize container
  return (StateTreeContainer top (mconcat newChildren))

padMaybes :: [a] -> [Maybe a]
padMaybes xs = map Just xs ++ repeat Nothing

instance IsContainer Gtk.ListBox (Bin Gtk.ListBoxRow Widget) where
  appendChild box _ widget' =
    Gtk.listBoxInsert box widget' (-1)
  replaceChild box _ i old new = do
    Gtk.widgetDestroy old
    Gtk.listBoxInsert box new i

instance IsContainer Gtk.Box BoxChild where
  appendChild box BoxChild {..} widget' =
    Gtk.boxPackStart box widget' expand fill padding
  replaceChild box boxChild' i old new = do
    Gtk.widgetDestroy old
    appendChild box boxChild' new
    Gtk.boxReorderChild box new i
