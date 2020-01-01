module Main where

import           Control.Concurrent
import           Control.Monad
import qualified GI.Gtk                              as Gtk
import           System.Exit
import           System.IO

import qualified GI.Gtk.Declarative.CustomWidgetTest as CustomWidget


main :: IO ()
main = do
  _ <- Gtk.init Nothing
  pass <- newEmptyMVar
  _ <- forkOS $ do
    results <- sequence [CustomWidget.tests]
    Gtk.mainQuit
    putMVar pass (and results)
  Gtk.main
  allPassed <- takeMVar pass
  unless allPassed $ do
    hPutStrLn stderr "Tests failed."
    exitFailure

