{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE RecordWildCards        #-}

-- | Implementations of 'Patchable' for common GTK+ container widgets.

module GI.Gtk.Declarative.Container
  ( BoxChild(..)
  , Children
  , PatchableContainer(..)
  , container
  )
where

import           Control.Monad.IO.Class                   ( MonadIO )
import           Control.Monad                            ( forM_
                                                          , void
                                                          )
import           GHC.Exts
import qualified Data.GI.Base                  as GI
import qualified Data.GI.Base.Attributes       as GI
import           Data.Int                                 ( Int32 )
import           Data.List                                ( zip4 )
import           Data.Typeable
import           Data.Word                                ( Word32 )
import qualified GI.Gtk                        as Gtk

import           GI.Gtk.Declarative.Markup
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.Props

class PatchableContainer widget children event | widget -> children where
  createChildrenIn :: widget -> children event -> IO ()
  patchChildrenIn :: widget -> children event -> children event -> IO ()

-- * Box

padMaybes :: [a] -> [Maybe a]
padMaybes xs = map Just xs ++ repeat Nothing

replaceInBox
  :: (Gtk.Widget -> IO ())
  -> Gtk.Box
  -> Int32
  -> Gtk.Widget
  -> Gtk.Widget
  -> IO ()
replaceInBox append box i old new = do
  Gtk.containerRemove box old
  append new
  Gtk.boxReorderChild box new i
  Gtk.widgetShowAll box

patchInBox
  :: Patchable child event
  => (Gtk.Box -> child event -> Gtk.Widget -> IO ())
  -> Gtk.Box
  -> [child event]
  -> [child event]
  -> IO ()
patchInBox appendChild box os' ns' = do
  cs <- Gtk.containerGetChildren box
  let maxLength = maximum [length cs, length os', length ns']
      indices   = [0 .. pred (fromIntegral maxLength)]
  forM_ (zip4 indices (padMaybes cs) (padMaybes os') (padMaybes ns')) $ \case

    -- In case we have a corresponding old and new virtual widget, we patch the
    -- GTK widget.
    (i, Just w, Just old, Just new) -> case patch old new of
      Modify modify -> modify w
      Replace createWidget ->
        replaceInBox (appendChild box new) box i w =<< createWidget
      Keep -> return ()

    -- When there is a new object, but there already exists a widget
    -- in the corresponding place, we need to replace the widget with
    -- one created from the object.
    (i, Just w, Nothing, Just new) ->
      replaceInBox (appendChild box new) box i w =<< create new

    -- When there is a new object, or one that lacks a corresponding GTK
    -- widget, create and add it.
    (_i, Nothing, _      , Just n ) -> create n >>= Gtk.containerAdd box

    -- When an object has been removed, remove the GTK widget from the
    -- container.
    (_i, Just w , Just _ , Nothing) -> Gtk.containerRemove box w

    -- When there are more old objects than GTK widgets, we can safely
    -- ignore the old objects.
    (_i, Nothing, Just _ , Nothing) -> return ()

    -- But, when there are stray GTK widgets without corresponding
    -- objects, something has gone wrong, and we clean that mess
    -- up by removing the GTK widgets.
    (_i, Just w , Nothing, Nothing) -> Gtk.containerRemove box w

    -- No more widgets or objects, we are done.
    (_i, Nothing, Nothing, Nothing) -> return ()

  Gtk.widgetQueueResize box

-- appendCreatedWidgetInBox :: Gtk.Box -> Markup event -> Gtk.Widget -> IO ()
-- appendCreatedWidgetInBox box _ = Gtk.containerAdd box

packCreatedBoxChildInBox :: Gtk.Box -> BoxChild event -> Gtk.Widget -> IO ()
packCreatedBoxChildInBox box BoxChild {..} widget =
  Gtk.boxPackStart box widget expand fill padding

newtype Children child event = Children [child event]

instance IsList (Children child event) where
  type Item (Children child event) = child event
  fromList = Children
  toList (Children xs) = xs

-- instance PatchableContainer Gtk.Box (Children Markup) event where
--   createChildrenIn box (Children children) = forM_ children $ \(Markup child) ->
--     appendCreatedWidgetInBox box (Markup child) =<< create child
--   patchChildrenIn widget (Children oldChildren) (Children newChildren) =
--     patchInBox appendCreatedWidgetInBox widget oldChildren newChildren

data BoxChild event = BoxChild { expand :: Bool, fill :: Bool, padding :: Word32, child :: Markup event }

instance Patchable BoxChild event where
  create = create . child
  patch b1 b2 = patch (child b1) (child b2)

instance PatchableContainer Gtk.Box (Children BoxChild) event where
  createChildrenIn box (Children children) =
    forM_ children $ \BoxChild {child = Markup child, ..} -> do
      widget <- create child
      Gtk.boxPackStart box widget expand fill padding
  patchChildrenIn widget (Children oldChildren) (Children newChildren) =
    patchInBox packCreatedBoxChildInBox widget oldChildren newChildren

-- * ScrolledWindow

instance PatchableContainer Gtk.ScrolledWindow Markup event where
  createChildrenIn box (Markup child) = create child >>= Gtk.containerAdd box
  patchChildrenIn scrolledWindow oldChild newChild = do
    viewport <- Gtk.containerGetChildren scrolledWindow
      >>= requireSingle "Viewport"
      >>= Gtk.unsafeCastTo Gtk.Viewport
    childWidget <- Gtk.containerGetChildren viewport
      >>= requireSingle "scrolled child"
    case patch oldChild newChild of
      Modify modify -> modify childWidget
      Replace createNew -> do
        Gtk.containerRemove viewport childWidget
        Gtk.containerAdd viewport =<< createNew
      Keep -> return ()
    where
      requireSingle what = \case
        [w] -> return w
        _ -> fail ("Expected a single " ++ what ++ " in the container.")

-- * Container object

data GtkContainer widget children a where
  GtkContainer
    :: (Typeable widget, Gtk.IsWidget widget)
    => (Gtk.ManagedPtr widget -> widget)
    -> [PropPair widget]
    -> children
    -> GtkContainer widget children a

instance Show (GtkContainer widget children a) where
  show = \case
    GtkContainer{} -> "GtkContainer"

extractAttrConstructOps
  :: PropPair widget -> [GI.AttrOp widget 'GI.AttrConstruct]
extractAttrConstructOps = \case
  (attr := value) -> pure (attr Gtk.:= value)
  _               -> mempty

extractAttrSetOps :: PropPair widget -> [GI.AttrOp widget 'GI.AttrSet]
extractAttrSetOps = \case
  (attr := value) -> pure (attr Gtk.:= value)
  _               -> mempty

addClass :: MonadIO m => Gtk.StyleContext -> PropPair widget -> m ()
addClass sc = \case
  Classes cs -> mapM_ (Gtk.styleContextAddClass sc) cs
  _          -> pure ()

removeClass :: MonadIO m => Gtk.StyleContext -> PropPair widget -> m ()
removeClass sc = \case
  Classes cs -> mapM_ (Gtk.styleContextRemoveClass sc) cs
  _          -> pure ()

addSignalHandler :: MonadIO m => widget -> PropPair widget -> m ()
addSignalHandler widget = \case
  OnSignal signal handler -> void (Gtk.on widget signal (handler widget))
  _                       -> pure ()


instance PatchableContainer widget children event => Patchable (GtkContainer widget (children event)) event where
  create (GtkContainer ctor props children) = do
    let attrOps = concatMap extractAttrConstructOps props
    widget <- Gtk.new ctor attrOps

    sc <- Gtk.widgetGetStyleContext widget
    mapM_ (addClass sc) props

    mapM_ (addSignalHandler widget) props

    createChildrenIn widget children
    Gtk.toWidget widget
  patch (GtkContainer _ oldProps oldChildren) (GtkContainer ctor newProps newChildren) =
    Modify $ \widget -> do

      w <- Gtk.unsafeCastTo ctor widget
      Gtk.set w (concatMap extractAttrSetOps newProps)

      sc <- Gtk.widgetGetStyleContext widget
      mapM_ (removeClass sc) oldProps
      mapM_ (addClass sc) newProps

      patchChildrenIn w oldChildren newChildren

container
  :: ( PatchableContainer widget children event
     , Typeable widget
     , Typeable children
     , Typeable event
     , Gtk.IsWidget widget
     )
  => (Gtk.ManagedPtr widget -> widget)
  -> [PropPair widget]
  -> children event
  -> Markup event
container ctor attrs = Markup . GtkContainer ctor attrs
