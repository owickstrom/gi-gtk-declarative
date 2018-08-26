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

-- | Implementation of 'Gtk.Box' as a declarative container.

module GI.Gtk.Declarative.Container.Box
  ( BoxChild(..)
  , boxChild
  )
where

import           Control.Monad                            ( forM_ )
import           Data.Typeable
import           Data.Word                                ( Word32 )
import GHC.TypeLits
import qualified GI.Gtk                        as Gtk

import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Markup
import           GI.Gtk.Declarative.Container
import           GI.Gtk.Declarative.Container.Patch

boxContainer :: CollectionContainer Gtk.Box BoxChild event
boxContainer =  CollectionContainer {..}
  where
    appendChild box BoxChild {..} widget' =
      Gtk.boxPackStart box widget' expand fill padding

    replaceChild box boxChild' i old new = do
      Gtk.containerRemove box old
      appendChild box boxChild' new
      Gtk.boxReorderChild box new i
      Gtk.widgetShowAll box

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
    patchInContainer boxContainer widget' oldChildren newChildren

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

