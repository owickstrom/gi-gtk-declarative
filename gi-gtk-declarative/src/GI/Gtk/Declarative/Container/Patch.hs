{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | This module implements the patch algorithm for containers.
module GI.Gtk.Declarative.Container.Patch
  ( IsContainer(..)
  , patchInContainer
  )
where

import           Data.Foldable                  ( foldMap )
import           Data.Vector                    ( Vector
                                                , (!?)
                                                )
import qualified Data.Vector                   as Vector
import qualified GI.Gtk                        as Gtk

import           GI.Gtk.Declarative.Container.Class
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
