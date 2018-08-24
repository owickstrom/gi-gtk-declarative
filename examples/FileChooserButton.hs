{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module FileChooserButton where

import qualified Data.Text                     as Text

import           GI.Gtk.Declarative            hiding (main)
import qualified GI.Gtk.Declarative            as Gtk
import           GI.Gtk.Declarative.App.Simple

data Model = Started (Maybe FilePath) | Done FilePath

data Event = FileSelectionChanged (Maybe FilePath) | ButtonClicked

view' :: Model -> Widget Event
view' (Done path) = node Label [#label := (Text.pack path <> " was selected.")]
view' (Started currentFile) =
  container Box [#orientation := OrientationVertical] $ do
    boxChild True True 0
      $ node Label [#label := maybe "No file yet." Text.pack currentFile]
    boxChild False False 10 $ node
      FileChooserButton
      [ onM #selectionChanged
            (fmap FileSelectionChanged . Gtk.fileChooserGetFilename)
      ]
    boxChild False False 10 $ node
      Button
      [#label := "Select", #tooltipText := "Hello!", on #clicked ButtonClicked]

update' :: Model -> Event -> (Model, IO (Maybe Event))
update' (Started _) (FileSelectionChanged p) = (Started p, return Nothing)
update' (Started (Just path)) ButtonClicked  = (Done path, return Nothing)
update' s _                                  = (s, return Nothing)

main :: IO ()
main = run "File Chooser Button" (Just (640, 480)) app (Started Nothing)
  where app = App {view = view', update = update', inputs = []}
