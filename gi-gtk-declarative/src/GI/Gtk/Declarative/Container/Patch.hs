{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

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
import           Data.Int                         (Int32)
import           Data.List                        (zip4)
import qualified GI.Gtk                           as Gtk

import           GI.Gtk.Declarative.Bin
import           GI.Gtk.Declarative.Container.Box
import           GI.Gtk.Declarative.Markup
import           GI.Gtk.Declarative.Patch


-- | Describes supported GTK+ containers and their specialized APIs for
-- appending and replacing child widgets.
class IsContainer container child | container -> child where
  -- | Append a child widget to the container.
  appendChild
    :: container    -- ^ Container widget
    -> child event  -- ^ Declarative child widget
    -> Gtk.Widget   -- ^ GTK child widget to append
    -> IO ()
  -- | Replace the child widget at the given index in the container.
  replaceChild
    :: container    -- ^ Container widget
    -> child event  -- ^ Declarative child widget
    -> Int32        -- ^ Index to replace at
    -> Gtk.Widget   -- ^ Old GTK widget to replace
    -> Gtk.Widget   -- ^ New GTK widget to replace with
    -> IO ()

-- | Patch all children in a container. This does not feature any ID checking,
-- as seen in React, so reordering children in a container can produce many
-- updates.
patchInContainer
  :: ( Gtk.IsWidget container
     , Gtk.IsContainer container
     , Patchable child
     , IsContainer container child
     )
  => ShadowState
  -> container
  -> [child e1]
  -> [child e2]
  -> IO ShadowState
patchInContainer (ShadowContainer top children) container os' ns' = do
  let maxLength = maximum [length children, length os', length ns']
      indices   = [0 .. pred (fromIntegral maxLength)]
  newChildren <-
    forM (zip4 indices (padMaybes children) (padMaybes os') (padMaybes ns'))
      $ \case

          -- In case we have a corresponding old and new declarative widget, we patch
          -- the GTK widget.
          (i, Just child, Just old, Just new) -> case patch child old new of
            Modify  modify       -> modify >> return [child]
            Replace createWidget -> do
              newChild <- createWidget
              replaceChild container
                           new
                           i
                           (shadowStateTopWidget child)
                           (shadowStateTopWidget newChild)
              return [newChild]
            Keep -> return [child]

          -- When there is a new declarative widget, but there already exists a GTK
          -- widget in the corresponding place, we need to replace the GTK widget with
          -- one created from the declarative widget.
          (i, Just child, Nothing, Just new) -> do
            newChild <- create new
            replaceChild container
                         new
                         i
                         (shadowStateTopWidget child)
                         (shadowStateTopWidget newChild)
            return [newChild]

          -- When there is a new declarative widget, or one that lacks a corresponding
          -- GTK widget, create and add it.
          (_i, Nothing, _, Just n) -> do
            newChild <- create n
            appendChild container n (shadowStateTopWidget newChild)
            return [newChild]

          -- When a declarative widget has been removed, remove the GTK widget from
          -- the container.
          (_i, Just child, Just _, Nothing) -> do
            Gtk.containerRemove container (shadowStateTopWidget child)
            return []

          -- When there are more old declarative widgets than GTK widgets, we can
          -- safely ignore the old declarative widgets.
          (_i, Nothing   , Just _ , Nothing) -> return []

          -- But, when there are stray GTK widgets without corresponding
          -- declarative widgets, something has gone wrong, and we clean that up by
          -- removing the GTK widgets.
          (_i, Just child, Nothing, Nothing) -> do
            Gtk.containerRemove container (shadowStateTopWidget child)
            -- TODO: destroy 'child' somehow?
            return []

          -- No more GTK widgets or declarative widgets, we are done.
          (_i, Nothing, Nothing, Nothing) -> return []

  Gtk.widgetQueueResize container
  return (ShadowContainer top (mconcat newChildren))
patchInContainer _ _ _ _ =
  error "Cannot patch container with non-container shadow state."

padMaybes :: [a] -> [Maybe a]
padMaybes xs = map Just xs ++ repeat Nothing

instance IsContainer Gtk.ListBox (Bin Gtk.ListBoxRow Widget) where
  appendChild box _ widget' = Gtk.listBoxInsert box widget' (-1)
  replaceChild box _ i old new = do
    Gtk.containerRemove box old
    Gtk.listBoxInsert box new i

instance IsContainer Gtk.Box BoxChild where
  appendChild box BoxChild {..} widget' =
    Gtk.boxPackStart box widget' expand fill padding
  replaceChild box boxChild' i old new = do
    Gtk.containerRemove box old
    appendChild box boxChild' new
    Gtk.boxReorderChild box new i
