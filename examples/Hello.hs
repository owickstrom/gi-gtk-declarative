{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

module Hello where

import           Control.Concurrent
import           Control.Monad
import           Data.Text          (Text)
import           GHC.Exts

import           GI.Gtk.Declarative hiding (main)
import qualified GI.Gtk.Declarative as Gtk

import           MainLoop

-- A very simple declarative user interface.
helloView :: (Text, Bool) -> Markup ()
helloView (who, flipped) = container Box [] $
  op
  [ BoxChild True True 0 (node Label [#label := "This is a sample application."])
  , BoxChild True True 0 (node Label [#label := who])
  ]
  where
    op :: [BoxChild ()] -> Children BoxChild ()
    op = fromList . if flipped then reverse else id

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

  -- We start a thread that sends new model values every second, with
  -- alternating True/False values.
  void . forkIO $
    mapM_
    (\(n, f) -> threadDelay 1000000 *> writeChan models ("Hello, " <> n, f))
    (cycle [("Joe", True), ("Mike", False)])

  -- And a thread for the main loop that listens for models, diffs the
  -- GUI, and re-renders the underlying GTK+ widgets when needed.
  void . forkIO $
    mainLoop window models helloView

  -- Let's do it!
  Gtk.main
