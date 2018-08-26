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
{-# LANGUAGE UndecidableInstances   #-}

module GI.Gtk.Declarative.Container.Patch where

import           Control.Monad             (forM_)
import           Data.Int                  (Int32)
import           Data.List                 (zip4)
import qualified GI.Gtk                    as Gtk

import           GI.Gtk.Declarative.Markup
import           GI.Gtk.Declarative.Patch

data CollectionContainer container child event =
  CollectionContainer { appendChild :: container -> child event -> Gtk.Widget -> IO ()
                      , replaceChild :: container -> child event -> Int32 -> Gtk.Widget -> Gtk.Widget -> IO ()
                      }

patchInContainer
  :: (Gtk.IsWidget container, Gtk.IsContainer container, Patchable child)
  => CollectionContainer container child e2
  -> container
  -> [child e1]
  -> [child e2]
  -> IO ()
patchInContainer CollectionContainer {..} container os' ns' = do
  cs <- Gtk.containerGetChildren container
  let maxLength = maximum [length cs, length os', length ns']
      indices   = [0 .. pred (fromIntegral maxLength)]
  forM_ (zip4 indices (padMaybes cs) (padMaybes os') (padMaybes ns')) $ \case

    -- In case we have a corresponding old and new virtual widget, we patch the
    -- GTK widget.
    (i, Just w, Just old, Just new) -> case patch old new of
      Modify modify -> modify w
      Replace createWidget ->
        replaceChild container new i w =<< createWidget
      Keep -> return ()

    -- When there is a new object, but there already exists a widget
    -- in the corresponding place, we need to replace the widget with
    -- one created from the object.
    (i, Just w, Nothing, Just new) ->
      replaceChild container new i w =<< create new

    -- When there is a new object, or one that lacks a corresponding GTK
    -- widget, create and add it.
    (_i, Nothing, _      , Just n ) -> create n >>= appendChild container n

    -- When an object has been removed, remove the GTK widget from the
    -- container.
    (_i, Just w , Just _ , Nothing) -> Gtk.containerRemove container w

    -- When there are more old objects than GTK widgets, we can safely
    -- ignore the old objects.
    (_i, Nothing, Just _ , Nothing) -> return ()

    -- But, when there are stray GTK widgets without corresponding
    -- objects, something has gone wrong, and we clean that mess
    -- up by removing the GTK widgets.
    (_i, Just w , Nothing, Nothing) -> Gtk.containerRemove container w

    -- No more widgets or objects, we are done.
    (_i, Nothing, Nothing, Nothing) -> return ()

  Gtk.widgetQueueResize container

padMaybes :: [a] -> [Maybe a]
padMaybes xs = map Just xs ++ repeat Nothing
