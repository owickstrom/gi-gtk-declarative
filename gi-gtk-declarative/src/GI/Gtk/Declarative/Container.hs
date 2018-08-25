{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE FlexibleContexts       #-}
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
  , boxChild
  , PatchableContainer(..)
  , container
  )
where

import           Control.Monad.IO.Class                   ( MonadIO )
import           Control.Monad                            ( forM_ )
import           Data.Maybe
import qualified Data.GI.Base                  as GI
import qualified Data.GI.Base.Attributes       as GI
import           Data.Int                                 ( Int32 )
import           Data.List                                ( zip4 )
import           Data.Typeable
import           Data.Word                                ( Word32 )
import GHC.TypeLits
import qualified GI.Gtk                        as Gtk

import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Markup
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.Props

class PatchableContainer widget children where
  createChildrenIn :: widget -> children -> IO ()
  patchChildrenIn :: widget -> children -> children -> IO ()

class ContainerEventSource widget children event | children -> event where
  subscribeChildren :: children -> widget -> (event -> IO ()) -> IO Subscription

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
  :: (Patchable child)
  => (Gtk.Box -> child e2 -> Gtk.Widget -> IO ())
  -> Gtk.Box
  -> [child e1]
  -> [child e2]
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

-- appendCreatedWidgetInBox :: Gtk.Box -> Widget event -> Gtk.Widget -> IO ()
-- appendCreatedWidgetInBox box _ = Gtk.containerAdd box

packCreatedBoxChildInBox :: Gtk.Box -> BoxChild event -> Gtk.Widget -> IO ()
packCreatedBoxChildInBox box BoxChild {..} widget' =
  Gtk.boxPackStart box widget' expand fill padding

data BoxChild event = BoxChild { expand :: Bool, fill :: Bool, padding :: Word32, child :: Widget event }

boxChild :: Bool -> Bool -> Word32 -> Widget event -> MarkupOf BoxChild event ()
boxChild expand fill padding child = widget BoxChild {..}

instance Patchable BoxChild where
  create = create . child
  patch b1 b2 = patch (child b1) (child b2)

instance EventSource (BoxChild event) event where
  subscribe BoxChild{..} = subscribe child

instance Typeable event => PatchableContainer Gtk.Box [BoxChild event] where
  createChildrenIn box children =
    forM_ children $ \BoxChild {child = child, ..} -> do
      widget' <- create child
      Gtk.boxPackStart box widget' expand fill padding
  patchChildrenIn widget' oldChildren newChildren =
    patchInBox packCreatedBoxChildInBox widget' oldChildren newChildren

instance Typeable event => PatchableContainer Gtk.Box (MarkupOf BoxChild event ()) where
  createChildrenIn box children =
    createChildrenIn box (runMarkup children)
  patchChildrenIn widget' oldChildren newChildren =
    patchChildrenIn widget' (runMarkup oldChildren) (runMarkup newChildren)

instance Typeable event => ContainerEventSource Gtk.Box [BoxChild event] event where
  subscribeChildren children box cb = do
    ws <- Gtk.containerGetChildren box
    foldMap (\(c, w) -> subscribe c w cb) (zip children ws)

instance Typeable event => ContainerEventSource Gtk.Box (MarkupOf BoxChild event ()) event where
  subscribeChildren children = subscribeChildren (runMarkup children)

instance TypeError (Text "The markup embedded in a Box needs to return " :<>: ShowType ()
                    :<>: Text ", not " :<>: ShowType [a] :<>: Text "."
                    :$$: Text "Did you perhaps use ‘mapM’ or ‘for’, instead of ‘mapM_’ or ‘for_’?")
  => ContainerEventSource Gtk.Box (MarkupOf BoxChild event [a]) event where
  subscribeChildren = undefined

-- * ScrolledWindow

requireSingle :: String -> [w] -> IO w
requireSingle what = \case
  [w] -> return w
  _   -> fail ("Expected a single " ++ what ++ " in the container.")

instance Typeable event => PatchableContainer Gtk.ScrolledWindow (Widget event) where
  createChildrenIn box child = create child >>= Gtk.containerAdd box
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

instance ContainerEventSource Gtk.ScrolledWindow (Widget event) event where
  subscribeChildren child scrolledWindow cb = do
    viewport <- Gtk.containerGetChildren scrolledWindow
      >>= requireSingle "Viewport"
      >>= Gtk.unsafeCastTo Gtk.Viewport
    childWidget <- Gtk.containerGetChildren viewport
      >>= requireSingle "scrolled child"
    subscribe child childWidget cb

-- * Container object

data GtkContainer widget children event where
  GtkContainer
    :: (Typeable widget, Gtk.IsWidget widget)
    => (Gtk.ManagedPtr widget -> widget)
    -> [PropPair widget event]
    -> children
    -> GtkContainer widget children event

instance Show (GtkContainer widget children a) where
  show = \case
    GtkContainer{} -> "GtkContainer"

extractAttrConstructOps
  :: PropPair widget event -> [GI.AttrOp widget 'GI.AttrConstruct]
extractAttrConstructOps = \case
  (attr := value) -> pure (attr Gtk.:= value)
  _               -> mempty

extractAttrSetOps :: PropPair widget event -> [GI.AttrOp widget 'GI.AttrSet]
extractAttrSetOps = \case
  (attr := value) -> pure (attr Gtk.:= value)
  _               -> mempty

addClass :: MonadIO m => Gtk.StyleContext -> PropPair widget event -> m ()
addClass sc = \case
  Classes cs -> mapM_ (Gtk.styleContextAddClass sc) cs
  _          -> pure ()

removeClass :: MonadIO m => Gtk.StyleContext -> PropPair widget event -> m ()
removeClass sc = \case
  Classes cs -> mapM_ (Gtk.styleContextRemoveClass sc) cs
  _          -> pure ()

addSignalHandler
  :: (Gtk.IsWidget widget, MonadIO m)
  => (event -> IO ())
  -> widget
  -> PropPair widget event
  -> m (Maybe ConnectedHandler)
addSignalHandler onEvent widget' = \case
  OnSignalPure signal handler -> do
    handlerId <- Gtk.on widget' signal (toGtkCallback handler onEvent)
    w         <- Gtk.toWidget widget'
    pure (Just (ConnectedHandler w handlerId))
  _ -> pure Nothing


instance (PatchableContainer widget children) => Patchable (GtkContainer widget children) where
  create (GtkContainer ctor props children) = do
    let attrOps = concatMap extractAttrConstructOps props
    widget' <- Gtk.new ctor attrOps

    sc <- Gtk.widgetGetStyleContext widget'
    mapM_ (addClass sc) props

    createChildrenIn widget' children
    Gtk.toWidget widget'
  patch (GtkContainer _ oldProps oldChildren) (GtkContainer ctor newProps newChildren) =
    Modify $ \widget' -> do

      w <- Gtk.unsafeCastTo ctor widget'
      Gtk.set w (concatMap extractAttrSetOps newProps)

      sc <- Gtk.widgetGetStyleContext widget'
      mapM_ (removeClass sc) oldProps
      mapM_ (addClass sc) newProps

      patchChildrenIn w oldChildren newChildren

instance ContainerEventSource widget children event
  => EventSource (GtkContainer widget children event) event where
  subscribe (GtkContainer ctor props children) widget' cb = do
    w <- Gtk.unsafeCastTo ctor widget'
    handlers' <- catMaybes <$> mapM (addSignalHandler cb w) props
    (<> Subscription handlers') <$> subscribeChildren children w cb

container
  :: ( PatchableContainer widget children
     , ContainerEventSource widget children event
     , Patchable (GtkContainer widget children)
     , Typeable widget
     , Typeable children
     , Typeable event
     , Gtk.IsWidget widget
     , FromWidget markup event
     )
  => (Gtk.ManagedPtr widget -> widget)
  -> [PropPair widget event]
  -> children
  -> markup
container ctor attrs = fromWidget . Widget . GtkContainer ctor attrs
