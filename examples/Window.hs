{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Window where

import           Control.Monad      (void)
import           GI.Gtk             (Label (..), Window (..))
import qualified GI.Gtk             as Gtk
import           GI.Gtk.Declarative

myWindow :: Widget ()
myWindow =
  bin Window [#defaultWidth := 400, #defaultHeight := 300] $
    widget Label [#label := "Nothing here yet."]

main :: IO ()
main = do
  void $ Gtk.init Nothing
  window <- create myWindow
  #showAll window
  void (Gtk.onWidgetDestroy window Gtk.mainQuit)
  Gtk.main
