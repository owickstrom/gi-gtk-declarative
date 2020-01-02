{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module GI.Gtk.Declarative.CustomWidgetTest where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception.Safe
import           Control.Monad                  ( replicateM_ )
import           Control.Monad.IO.Class
import           Data.Function                  ( (&) )
import qualified Data.HashSet                  as HashSet
import qualified Data.Text                     as Text
import           Data.Vector                    ( Vector )
import qualified GI.GObject                    as GI
import qualified GI.Gtk                        as Gtk

import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.State

import           GI.Gtk.Declarative.TestUtils

prop_sets_the_button_label = property $ do
  start       <- forAll (Gen.int (Range.linear 0 10))
  clicks      <- forAll (Gen.int (Range.linear 0 10))

  buttonLabel <- runUI . bracket (Gtk.new Gtk.Window []) #destroy $ \window ->
    do
      let markup = testWidget [] start
      first <- create markup
      btn   <- someStateWidget first >>= Gtk.unsafeCastTo Gtk.Button & liftIO
      #add window btn
      sub <- subscribe markup first (const (pure ()))
      Gtk.widgetShowAll window
      replicateM_ clicks (Gtk.buttonClicked btn)
      cancel sub
      Gtk.get btn #label

  let expectedLabel = Text.pack (show (start + clicks))
  expectedLabel === buttonLabel

prop_emits_correct_number_of_click_events = property $ do
  start  <- forAll (Gen.int (Range.linear 0 10))
  clicks <- forAll (Gen.int (Range.linear 0 10))

  values <- liftIO (newTBQueueIO (fromIntegral clicks))
  runUI . bracket (Gtk.new Gtk.Window []) #destroy $ \window -> do
    let markup = testWidget [] start
    first <- create markup
    btn   <- someStateWidget first >>= Gtk.unsafeCastTo Gtk.Button & liftIO
    #add window btn
    sub <- subscribe markup first (atomically . writeTBQueue values)
    Gtk.widgetShowAll window
    replicateM_ clicks (Gtk.buttonClicked btn)
    cancel sub

  let expectedValues = take clicks [succ start ..]
  actualValues <- liftIO (atomically (flushTBQueue values))
  expectedValues === actualValues

prop_sets_classes = property $ do
  let genClasses =
        Gen.list (Range.linear 0 5) (Gen.text (Range.linear 1 5) Gen.alphaNum)
  initialClasses                <- forAll genClasses
  finalClasses                  <- forAll genClasses

  (classesBefore, classesAfter) <-
    runUI . bracket (Gtk.new Gtk.Window []) #destroy $ \window -> do
      let markup1 = testWidget [classes initialClasses] 0
          markup2 = testWidget [classes finalClasses] 0
      first <- create markup1
      btn   <- liftIO (someStateWidget first >>= Gtk.unsafeCastTo Gtk.Button)
      #add window btn
      Gtk.widgetShowAll window
      sc           <- #getStyleContext btn
      beforeUpdate <- #listClasses sc
      _second      <- patch' first markup1 markup2
      afterUpdate  <- #listClasses sc
      pure (beforeUpdate, afterUpdate)

  HashSet.fromList ("text-button" : initialClasses)
    === HashSet.fromList classesBefore
  HashSet.fromList ("text-button" : finalClasses)
    === HashSet.fromList classesAfter

-- * Test widget and helpers

testWidget :: Vector (Attribute Gtk.Button Int) -> Int -> Widget Int
testWidget customAttributes customParams = Widget (CustomWidget { .. })
 where
  customWidget = Gtk.Button
  customCreate start = do
    clicks <- newMVar start
    btn    <- Gtk.new Gtk.Button [#label Gtk.:= Text.pack (show start)]
    return (btn, clicks)

  customPatch :: Int -> Int -> MVar Int -> CustomPatch Gtk.Button (MVar Int)
  customPatch _ new clicks = CustomModify $ \btn -> do
    -- putMVar clicks new
    Gtk.set btn [#label Gtk.:= Text.pack (show new)]
    return clicks

  customSubscribe
    :: Int -> MVar Int -> Gtk.Button -> (Int -> IO ()) -> IO Subscription
  customSubscribe _params clicks btn cb = do
    h <- Gtk.on btn #clicked $ do
      current <- modifyMVar clicks $ \x -> pure (succ x, succ x)
      cb current
      Gtk.set btn [#label Gtk.:= Text.pack (show current)]
    return (fromCancellation (GI.signalHandlerDisconnect btn h))

-- * Test collection

tests :: IO Bool
tests = checkParallel $$(discover)
