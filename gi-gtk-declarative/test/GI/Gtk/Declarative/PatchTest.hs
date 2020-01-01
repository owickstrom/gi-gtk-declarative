{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module GI.Gtk.Declarative.PatchTest where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception.Safe
import Control.Monad (replicateM_)
import Control.Monad (foldM)
import Control.Monad.IO.Class
import Data.Foldable (traverse_)
import Data.Function ((&))
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Tree
import Data.Vector (Vector)
import qualified GI.GLib.Constants as GLib
import qualified GI.GObject as GI
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative
import GI.Gtk.Declarative.EventSource
import qualified GI.Gtk.Declarative.Gen as Gen
import GI.Gtk.Declarative.State
import GI.Gtk.Declarative.TestUtils
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

prop_history_of_prior_patches_has_no_effect_on_resulting_widget = property $ do
  (first : intermediate) <- forAll (Gen.list (Range.linear 1 100) Gen.widget)
  last <- forAll Gen.widget
  cover 10 "nested widgets" (case last of Gen.TestBox _ (_:_) -> True; _ -> False)
  -- Directly creating the 'last' widget.
  widget <- assertRight =<< patchAllInNewWindow last []
  -- Creating and patching all prior widgets before patching with the 'last' widget.
  widget' <- assertRight =<< patchAllInNewWindow first (intermediate <> pure last)
  -- They should be the same.
  widget === widget'

assertRight :: MonadTest m => Either Text a -> m a
assertRight (Left err) = annotate (Text.unpack err) >> failure
assertRight (Right a) = pure a

patchAll :: SomeState -> Widget event -> [Widget event] -> IO SomeState
patchAll s1 w ws = fst <$> foldM (\(s, w1) w2 -> (,w2) <$> patch' s w1 w2) (s1, w) ws

patchAllInNewWindow :: MonadIO m => Gen.TestWidget -> [Gen.TestWidget] -> m (Either Text Gen.TestWidget)
patchAllInNewWindow first rest = runUI . bracket (Gtk.new Gtk.Window []) #destroy $ \window -> do
  firstState <- create (Gen.toWidget first)
  widget <- liftIO (someStateWidget firstState)
  #add window widget
  Gtk.widgetShowAll window
  lastState <- patchAll firstState (Gen.toWidget first) (map Gen.toWidget rest)
  Gen.fromGtkWidget =<< someStateWidget lastState

-- * Test collection

tests :: IO Bool
tests =
  checkParallel $$(discover)
