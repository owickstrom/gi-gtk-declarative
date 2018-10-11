{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Window where

import           Control.Monad      (void)
import           GI.Gtk             (Label (..), Window (..))
import qualified GI.Gtk             as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.EventSource (subscribe)

data Event = Closed

myWindow :: Widget Event
myWindow =
  bin
    Window
    [ on #deleteEvent (const (Closed, True))
    , #defaultWidth := 400, #defaultHeight := 300
    , #title := "Closable Window"
    ]
    $ widget Label [#label := "You can close me."]

main :: IO ()
main = do
  void $ Gtk.init Nothing
  window <- create myWindow
  _ <- subscribe myWindow window $ \Closed -> Gtk.mainQuit
  #showAll window
  Gtk.main
