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

import           Control.Exception.Safe
import           Control.Monad                  ( foldM )
import           Control.Monad.IO.Class
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.TestWidget
import           GI.Gtk.Declarative.State
import           GI.Gtk.Declarative.TestUtils
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

prop_history_of_prior_patches_has_no_effect_on_resulting_widget = property $ do
  (first : intermediate) <- forAll (Gen.list (Range.linear 1 100) genTestWidget)
  last'                  <- forAll genTestWidget
  cover 10 "prior include nested widgets" (any isNested (first : intermediate))
  cover 5  "last is nested widget"        (isNested last')
  -- Directly creating the 'last' widget.
  direct      <- assertRight =<< patchAllInNewWindow last' []
  -- Creating and patching all prior widgets before patching with the 'last' widget.
  afterOthers <- assertRight
    =<< patchAllInNewWindow first (intermediate <> pure last')
  -- They (converted back to TestWidgets) should be the same.
  direct === afterOthers
  -- And the result should be equal to the TestWidget we started with.
  afterOthers === setDefaults last'

assertRight :: MonadTest m => Either Text a -> m a
assertRight (Left  err) = annotate (Text.unpack err) >> failure
assertRight (Right a  ) = pure a

patchAll :: SomeState -> Widget event -> [Widget event] -> IO SomeState
patchAll s1 w ws =
  fst <$> foldM (\(s, w1) w2 -> (, w2) <$> patch' s w1 w2) (s1, w) ws

patchAllInNewWindow
  :: MonadIO m => TestWidget -> [TestWidget] -> m (Either Text TestWidget)
patchAllInNewWindow first rest =
  runUI . bracket (Gtk.new Gtk.Window []) #destroy $ \window -> do
    firstState <- create (toTestWidget first)
    #add window =<< someStateWidget firstState
    Gtk.widgetShowAll window
    lastState <- patchAll firstState
                          (toTestWidget first)
                          (map toTestWidget rest)
    fromGtkWidget =<< someStateWidget lastState

-- * Test collection

tests :: IO Bool
tests = checkParallel $$(discover)
