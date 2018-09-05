{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module FileChooserButton where

import qualified Data.Text                     as Text

import           GI.Gtk                        (Box (..), Button (..),
                                                FileChooserButton (..),
                                                Label (..), Orientation (..),
                                                fileChooserGetFilename)
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

data State = Started (Maybe FilePath) | Done FilePath

data Event = FileSelectionChanged (Maybe FilePath) | ButtonClicked

view' :: State -> Widget Event
view' (Done path) =
  widget Label [#label := (Text.pack path <> " was selected.")]
view' (Started currentFile) =
  container Box [#orientation := OrientationVertical] $ do
    boxChild True True 0
      $ widget Label [#label := maybe "No file yet." Text.pack currentFile]
    boxChild False False 10 $ widget
      FileChooserButton
      [ onM #selectionChanged
            (fmap FileSelectionChanged . fileChooserGetFilename)
      ]
    boxChild False False 10 $ widget
      Button
      [ #label := "Select"
      , #tooltipText := "Select the chosen file"
      , on #clicked ButtonClicked
      ]

update' :: State -> Event -> Transition State Event
update' (Started _) (FileSelectionChanged p) = Transition (Started p) (return Nothing)
update' (Started (Just path)) ButtonClicked = Transition (Done path) (return Nothing)
update' s _ = Transition s (return Nothing)

main :: IO ()
main =
  run
    "File Chooser Button"
    (Just (640, 480))
    App
    { view = view'
    , update = update'
    , inputs = []
    , initialState = Started Nothing
    }
