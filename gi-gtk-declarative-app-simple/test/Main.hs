{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists  #-}
module Main where

import           Control.Concurrent            (threadDelay)
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           Pipes
import           System.Timeout
import           Test.Hspec

main :: IO ()
main = hspec $
  describe "run" $ do
    it "processes events from inputs" $
      runApp app { inputs = [yield IncState >> yield Close]} >>= (`shouldBe` Just 1)
    it "finishes app on Exit when input still going" $ do
      runApp app { inputs = [closeLoop] } >>= (`shouldBe` Just 0)
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
        runApp app { update = \s _ -> Transition s (pure $ error "oh no")
                   , inputs = [yield ThrowError]
                   } `shouldThrow` errorCall "oh no"
      it "when the io is an exception" $
        runApp app { update = \s _ -> Transition s (error "oh no")
                   , inputs = [yield ThrowError]
                   } `shouldThrow` errorCall "oh no"
      it "when the newly generated event is an exception" $
        -- Note: forcing the event by pattern matching is important to raise the exception
        runApp app { update = \s ThrowError -> Transition s (pure $ Just (error "oh no"))
                   , inputs = [yield ThrowError]
                   } `shouldThrow` errorCall "oh no"
  where
    app = App
      { update = update'
      , view = view'
      , inputs = []
      , initialState = 0
      }
    runApp = timeout 1000000 . run
    closeLoop = do
      yield Close
      liftIO (threadDelay 1000000)
      closeLoop

type AppState = Int

data AppEvent = IncState | ThrowError | Close

view' :: AppState -> AppView Gtk.Window AppEvent
view' _ = bin Gtk.Window [] (widget Gtk.Label [])

update' :: AppState -> AppEvent -> Transition AppState AppEvent
update' state = \case
  IncState   -> Transition (state + 1) (pure Nothing)
  ThrowError -> error "oh no"
  Close      -> Exit
