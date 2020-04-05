{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists  #-}
module Main where

import           Control.Monad                 (void)
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           Pipes
import           System.Timeout
import           Test.Hspec

main :: IO ()
main = hspec $
  describe "run" $ do
    it "propagates exceptions from view function" $
      runApp app {view = const (error "oh no")} `shouldThrow` errorCall "oh no"
    it "propagates exceptions from update handler itself" $
      runApp app {inputs = [yield ThrowError]} `shouldThrow` errorCall "oh no"
    it "propagates exceptions from the pipeline itself" $
      runApp app {inputs = [error "oh no"]} `shouldThrow` errorCall "oh no"
  where
    app = App
      { update = update'
      , view = view'
      , inputs = []
      , initialState = ()
      }
    runApp = timeout 1000000 . void . run

data AppEvent = ThrowError

view' :: () -> AppView Gtk.Window AppEvent
view' () = bin Gtk.Window [] (widget Gtk.Label [])

update' :: () -> AppEvent -> Transition () AppEvent
update' () ThrowError = error "oh no"
