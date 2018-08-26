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

-- | Implementation of 'Gtk.Box' as a declarative container.

module GI.Gtk.Declarative.Container.ListBox
  ( ListBoxRow(..)
  , listBoxRow
  )
where

import           Control.Monad                      (forM_)
import           Data.Typeable
import           GHC.TypeLits
import qualified GI.Gtk                             as Gtk

import           GI.Gtk.Declarative.Container
import           GI.Gtk.Declarative.Container.Patch
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Markup
import           GI.Gtk.Declarative.Patch

listBoxContainer :: CollectionContainer Gtk.ListBox ListBoxRow event
listBoxContainer =  CollectionContainer {..}
  where
    appendChild box ListBoxRow {..} widget' =
      Gtk.listBoxInsert box widget' (-1)
    replaceChild box ListBoxRow {..} i old new = do
      Gtk.containerRemove box old
      Gtk.listBoxInsert box new i
      Gtk.widgetShowAll box

data ListBoxRow event = ListBoxRow { selectable :: Bool, activatable :: Bool, listBoxRowChild :: Widget event }

listBoxRow :: Bool -> Bool -> Widget event -> MarkupOf ListBoxRow event ()
listBoxRow selectable activatable listBoxRowChild = widget ListBoxRow {..}

requireSingle :: String -> [w] -> IO w
requireSingle what = \case
  [w] -> return w
  _   -> fail ("Expected a single " ++ what ++ " in the container.")

instance Patchable ListBoxRow where
  create = create . listBoxRowChild
  patch r1 r2 = do
    case patch (listBoxRowChild r1) (listBoxRowChild r2) of
      Modify modify     ->
        Modify $ \w -> do
          boxRow <- Gtk.unsafeCastTo Gtk.ListBoxRow w
          Gtk.listBoxRowSetSelectable boxRow (selectable r2)
          Gtk.listBoxRowSetActivatable boxRow (activatable r2)
          rowChild <-
            Gtk.containerGetChildren boxRow
            >>= requireSingle "ListBoxRow child"
          modify rowChild
      Replace createNew ->
        Replace $ do
          child <- createNew
          boxRow <- Gtk.listBoxRowNew
          Gtk.listBoxRowSetSelectable boxRow (selectable r2)
          Gtk.listBoxRowSetActivatable boxRow (activatable r2)
          Gtk.containerAdd boxRow child
          Gtk.toWidget boxRow
      Keep              -> Keep

instance EventSource (ListBoxRow event) event where
  subscribe ListBoxRow{..} w cb = do
    boxRow <- Gtk.unsafeCastTo Gtk.ListBoxRow w
    rowChild <-
      Gtk.containerGetChildren boxRow
      >>= requireSingle "ListBoxRow child"
    subscribe listBoxRowChild rowChild cb

instance Typeable event => PatchableContainer Gtk.ListBox [ListBoxRow event] where
  createChildrenIn box children =
    forM_ children $ \ListBoxRow {..} -> do
      child <- create listBoxRowChild
      boxRow <- Gtk.listBoxRowNew
      Gtk.listBoxRowSetSelectable boxRow selectable
      Gtk.listBoxRowSetActivatable boxRow activatable
      Gtk.containerAdd boxRow child
      Gtk.listBoxInsert box boxRow (-1)
  patchChildrenIn widget' oldChildren newChildren =
    patchInContainer listBoxContainer widget' oldChildren newChildren

instance Typeable event => PatchableContainer Gtk.ListBox (MarkupOf ListBoxRow event ()) where
  createChildrenIn box children =
    createChildrenIn box (runMarkup children)
  patchChildrenIn widget' oldChildren newChildren =
    patchChildrenIn widget' (runMarkup oldChildren) (runMarkup newChildren)

instance Typeable event => ContainerEventSource Gtk.ListBox [ListBoxRow event] event where
  subscribeChildren children box cb = do
    ws <- Gtk.containerGetChildren box
    foldMap (\(c, w) -> subscribe c w cb) (zip children ws)

instance Typeable event => ContainerEventSource Gtk.ListBox (MarkupOf ListBoxRow event ()) event where
  subscribeChildren children = subscribeChildren (runMarkup children)

instance TypeError (Text "The markup embedded in a Box needs to return " :<>: ShowType ()
                    :<>: Text ", not " :<>: ShowType [a] :<>: Text "."
                    :$$: Text "Did you perhaps use ‘mapM’ or ‘for’, instead of ‘mapM_’ or ‘for_’?")
  => ContainerEventSource Gtk.Box (MarkupOf ListBoxRow event [a]) event where
  subscribeChildren = undefined
