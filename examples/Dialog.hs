{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Dialog where

import           Control.Monad      (void)
import           GI.Gtk             (Label (..), Button(..))
import qualified GI.Gtk             as Gtk
import           GI.Gtk.Declarative

data Event = Yes | No

myWindow :: Widget Event
myWindow =
  dialog [#defaultWidth := 400, #defaultHeight := 300] $ do
    dialogContent (widget Button [#label := "Yes", on #clicked Yes])
    dialogContent (widget Button [#label := "Yes", on #clicked No])

main :: IO ()
main = do
  void $ Gtk.init Nothing
  window <- create myWindow
  #showAll window
  void (Gtk.onWidgetDestroy window Gtk.mainQuit)
  Gtk.main
