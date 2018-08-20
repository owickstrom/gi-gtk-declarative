{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE RecordWildCards   #-}

module FileChooserButton where

import           Control.Concurrent
import           Control.Monad
import qualified Data.Text                     as Text

import           GI.Gtk.Declarative                hiding ( main )
import qualified GI.Gtk.Declarative            as Gtk

import           MainLoop


data Model = Started (Maybe FilePath) | Done FilePath

data Event = FileSelectionChanged (Maybe FilePath) | ButtonClicked

view' :: Model -> Markup Event
view' (Done path) = node Label [#label := (Text.pack path <> " was selected.")]
view' (Started currentFile) = container
  Box
  [#orientation := OrientationVertical]
  [ BoxChild
    True
    True
    0
    (node Label [#label := maybe "No file yet." Text.pack currentFile])
  , BoxChild
    False
    False
    10
    (node
      FileChooserButton
      [ onM #selectionChanged
            (fmap FileSelectionChanged . Gtk.fileChooserGetFilename)
      ]
    )
  , BoxChild
    False
    False
    10
    (node
      Button
      [#label := "Select", #tooltipText := "Hello!", on #clicked ButtonClicked]
    )
  ]

update' :: Model -> Event -> (Model, IO (Maybe Event))
update' (Started _) (FileSelectionChanged p) = (Started p, return Nothing)
update' (Started (Just path)) ButtonClicked = (Done path, return Nothing)
update' s _ = (s, return Nothing)

main :: IO ()
main = do
  -- Basic setup.
  void $ Gtk.init Nothing
  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  void (Gtk.onWidgetDestroy window mainQuit)

  -- Customize the top window.
  Gtk.windowSetTitle window "Sample gi-gtk-declarative app!"
  Gtk.windowResize window 640 480

  input' <- newChan

  let app = App {view = view', update = update', input = input'}

  void . forkIO $ runInWindow window app (Started Nothing)

  -- Let's do it!
  Gtk.main
