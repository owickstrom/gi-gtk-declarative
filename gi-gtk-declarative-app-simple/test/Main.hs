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
    -- Propagating exception from the view/update/inputs even is
    -- important to crash the application instead of keeping it in a
    -- weird state.
    it "propagates exceptions from view function" $
      runApp app {view = const (error "oh no")} `shouldThrow` errorCall "oh no"
    it "propagates exceptions from update handler itself" $
      runApp app {inputs = [yield ThrowError]} `shouldThrow` errorCall "oh no"
    it "propagates exceptions from the pipeline itself" $
      runApp app {inputs = [error "oh no"]} `shouldThrow` errorCall "oh no"
    describe "propagates exceptions from the Transition" $ do
      it "when the maybe is an exception" $
        runApp app { update = \() _ -> Transition () (pure $ error "oh no")
                   , inputs = [yield ThrowError]
                   } `shouldThrow` errorCall "oh no"
      it "when the io is an exception" $
        runApp app { update = \() _ -> Transition () (error "oh no")
                   , inputs = [yield ThrowError]
                   } `shouldThrow` errorCall "oh no"
      it "when the newly generated even is an exception" $
        -- Note: forcing the event by pattern matching is important to raise the exception
        runApp app { update = \() ThrowError -> Transition () (pure $ Just (error "oh no"))
                   , inputs = [yield ThrowError]
                   } `shouldThrow` errorCall "oh no"
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
