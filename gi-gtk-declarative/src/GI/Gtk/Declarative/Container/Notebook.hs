{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

-- | Implementation of 'Gtk.Notebook' as a declarative container.
module GI.Gtk.Declarative.Container.Notebook
  ( Page
  , page
  , pageWithTab
  , notebook
  )
where

import           Control.Monad                  ( void )
import           Data.Maybe                     ( isNothing )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Vector                    ( Vector )
import           GHC.Ptr                        ( nullPtr )
import qualified GI.GLib                       as GLib
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative.Attributes
import           GI.Gtk.Declarative.Container
import           GI.Gtk.Declarative.Container.Class
import           GI.Gtk.Declarative.SingleWidget
import           GI.Gtk.Declarative.Widget

-- | Describes a page to be added to a 'Notebook'
data Page event =
  Page
    { tabLabel :: Widget event
    , child    :: Widget event
    }

-- | Create a page with a textual label and an arbitrary content widget.
page :: Text -> Widget event -> Page event
page label = pageWithTab (widget Gtk.Label [#label := label])

-- | Create a page with arbitrary widgets for both label and content.
pageWithTab :: Widget event -> Widget event -> Page event
pageWithTab = Page

-- | Create a 'Notebook' by combining multiple pages.
notebook
  :: Vector (Attribute Gtk.Notebook event)
  -> Vector (Page event)
  -> Widget event
notebook attrs children =
  let childrenAndTabs = children >>= (\Page {..} -> [child, tabLabel])
  in  container Gtk.Notebook attrs childrenAndTabs

instance ToChildren Gtk.Notebook Vector Widget

instance IsContainer Gtk.Notebook Widget where
  appendChild parent _ new = do
    lastPage <- Gtk.notebookGetNthPage parent (-1)
    case lastPage of
      -- this is the first page to be added
      Nothing -> do
        void $ Gtk.notebookAppendPage parent new (Nothing :: Maybe Gtk.Widget)
      Just p -> do
        label <- Gtk.notebookGetTabLabel parent p
        if isNothing label
            -- this page must already have a child, we just need to set the label
          then do
            Gtk.notebookSetTabLabel parent p (Just new)
          else do
            void $ Gtk.notebookAppendPage parent
                                          new
                                          (Nothing :: Maybe Gtk.Widget)
  replaceChild parent _ i old new = do
    let i' = i `div` 2
    pageI <- Gtk.notebookGetNthPage parent i'
    case pageI of
      Nothing -> do
        GLib.logDefaultHandler
          (Just "gi-gtk-declarative")
          [GLib.LogLevelFlagsLevelError]
          (Just
          $ "GI.Gtk.Declarative.Container.Notebook.replaceChild called with an index where there is no child: "
          <> pack (show i)
          )
          nullPtr
      Just p -> do
        if i `mod` 2 == 0
          then do
            label <- Gtk.notebookGetTabLabel parent p -- we have to replace the child
            Gtk.widgetDestroy old
            void $ Gtk.notebookInsertPage parent new label i'
          else do
            Gtk.notebookSetTabLabel parent p (Just new) -- we have to replace the label
