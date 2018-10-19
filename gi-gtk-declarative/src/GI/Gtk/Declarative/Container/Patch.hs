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
module GI.Gtk.Declarative.Container.Patch (IsContainer(..), patchInContainer) where

import           Control.Monad                    (forM_)
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
  => container
  -> [child e1]
  -> [child e2]
  -> IO ()
patchInContainer container os' ns' = do
  cs <- Gtk.containerGetChildren container
  let maxLength = maximum [length cs, length os', length ns']
      indices   = [0 .. pred (fromIntegral maxLength)]
  forM_ (zip4 indices (padMaybes cs) (padMaybes os') (padMaybes ns')) $ \case

    -- In case we have a corresponding old and new declarative widget, we patch
    -- the GTK widget.
    (i, Just w, Just old, Just new) -> case patch old new of
      Modify  modify       -> modify w
      Replace createWidget -> replaceChild container new i w =<< createWidget
      Keep                 -> return ()

    -- When there is a new declarative widget, but there already exists a GTK
    -- widget in the corresponding place, we need to replace the GTK widget with
    -- one created from the declarative widget.
    (i, Just w, Nothing, Just new) ->
      replaceChild container new i w =<< create new

    -- When there is a new declarative widget, or one that lacks a corresponding
    -- GTK widget, create and add it.
    (_i, Nothing, _      , Just n ) -> create n >>= appendChild container n

    -- When an declarative widget has been removed, remove the GTK widget from
    -- the container.
    (_i, Just w , Just _ , Nothing) -> Gtk.containerRemove container w

    -- When there are more old declarative widgets than GTK widgets, we can
    -- safely ignore the old declarative widgets.
    (_i, Nothing, Just _ , Nothing) -> return ()

    -- But, when there are stray GTK widgets without corresponding
    -- declarative widgets, something has gone wrong, and we clean that up by
    -- removing the GTK widgets.
    (_i, Just w , Nothing, Nothing) -> Gtk.containerRemove container w

    -- No more GTK widgets or declarative widgets, we are done.
    (_i, Nothing, Nothing, Nothing) -> return ()

  Gtk.widgetQueueResize container

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
