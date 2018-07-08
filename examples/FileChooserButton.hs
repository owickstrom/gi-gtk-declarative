{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.Text          (Text)
import qualified Data.Text          as Text

import           GI.Gtk.Declarative hiding (main)
import qualified GI.Gtk.Declarative as Gtk

import           MainLoop


data FileSelected = FileSelected FilePath
data ButtonClicked = ButtonClicked

-- A very simple declarative user interface.
fileChooserView ::  Chan FileSelected -> Chan ButtonClicked ->Maybe FilePath -> Markup
fileChooserView fileSelected buttonClicked currentFile =
  container Box [#orientation := OrientationVertical] $
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
         [on #selectionChanged onFileSelectionChanged])
  , BoxChild
      False
      False
      10
      (node
         Button
         [#label := "Select", on #clicked (const (writeChan buttonClicked ButtonClicked))])
  ]
  where
    onFileSelectionChanged w =
      Gtk.fileChooserGetFilename w >>= \case
        Just file -> writeChan fileSelected (FileSelected file)
        Nothing -> pure ()

main :: IO ()
main = do
  -- Basic setup.
  void $ Gtk.init Nothing
  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  void (Gtk.onWidgetDestroy window mainQuit)

  -- Customize the top window.
  Gtk.windowSetTitle window "Sample gi-gtk-declarative app!"
  Gtk.windowResize window 640 480

  -- A channel of "model" values.
  models <- newChan
  -- And the initial state.
  writeChan models Nothing

  -- A channel for each event type.
  fileSelected <- newChan
  buttonClicked <- newChan

  -- Here's the main flow of the application, where we first await a
  -- file to be selected, and then the button to be clicked for
  -- confirmation.
  void . forkIO $ do
    FileSelected f <- readChan fileSelected
    writeChan models (Just "Now, click the button.")
    ButtonClicked <- readChan buttonClicked
    writeChan models (Just ("You have selected a file: " <> f))

  -- And a thread for the main loop that listens for models, diffs the
  -- GUI, and re-renders the underlying GTK+ widgets when needed.
  void . forkIO $
    mainLoop window models (fileChooserView fileSelected buttonClicked)

  -- Let's do it!
  Gtk.main
