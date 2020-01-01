{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module GI.Gtk.Declarative.TestWidget where

import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative hiding (widget)
import qualified GI.Gtk.Declarative as Gtk (widget)
import GI.Gtk.Declarative.EventSource
import GI.Gtk.Declarative.State
import Hedgehog hiding (label)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Prelude

-- | GTK widgets cannot (in any practical, generic sense) be compared and shown
-- in tests, so we represent widgets in property tests using this data
-- structure. We convert between this representation, declarative widgets, and
-- instantiated GTK widgets.
data TestWidget
  = TestButton Text
  | TestLabel Text
  | TestScrolledWindow TestWidget
  | TestBox Gtk.Orientation [TestWidget]
  deriving (Eq, Show)

isNested :: TestWidget -> Bool
isNested = \case
  TestButton{} -> False
  TestLabel{} -> False
  (TestScrolledWindow _) -> True
  (TestBox _ children) -> not (null children)

toTestWidget :: TestWidget -> Widget Void
toTestWidget = \case
  TestLabel label -> Gtk.widget Gtk.Label [#label := label]
  TestButton label -> Gtk.widget Gtk.Button [#label := label]
  TestScrolledWindow child -> bin Gtk.ScrolledWindow [] (toTestWidget child)
  TestBox orientation children ->
    container
      Gtk.Box
      [#orientation := orientation]
      (Vector.map (BoxChild defaultBoxChildProperties . toTestWidget) (Vector.fromList children))

fromGtkWidget :: (MonadIO m) => Gtk.Widget -> m (Either Text TestWidget)
fromGtkWidget = runExceptT . go
  where
    go :: (MonadIO m) => Gtk.Widget -> ExceptT Text m TestWidget
    go w = do
      name <- #getName w
      case name of
        "GtkButton" -> withCast w Gtk.Button (\btn -> TestButton <$> Gtk.get btn #label)
        "GtkLabel" -> withCast w Gtk.Label (\lbl -> TestLabel <$> Gtk.get lbl #label)
        "GtkScrolledWindow" -> withCast w Gtk.ScrolledWindow $ \win -> do
          w' <- #getChild win >>= maybe (throwError "No viewport in scrolled window") pure
          withCast w' Gtk.Viewport $ \viewport -> do
            child <- #getChild viewport >>= maybe (throwError "No child in scrolled window") pure
            TestScrolledWindow <$> go child
        "GtkBox" -> withCast w Gtk.Box $ \box -> do
          childWidgets <- traverse go =<< #getChildren box
          orientation <- Gtk.get box #orientation
          pure (TestBox orientation childWidgets)
        _ -> throwError ("Unsupported TestWidget: " <> name)
    withCast ::
      (MonadIO m, Gtk.GObject w, Gtk.GObject w') =>
      w ->
      (Gtk.ManagedPtr w' -> w') ->
      (w' -> ExceptT Text m a) ->
      ExceptT Text m a
    withCast w ctor f = liftIO (Gtk.castTo ctor w) >>= \case
      Just w' -> f w'
      Nothing -> throwError "Failed to cast widget"

-- * Generators

genTestWidget :: Gen TestWidget
genTestWidget =
  Gen.recursive
    Gen.choice
    [ genLabel,
      genButton
    ]
    ( subwidgets (\ws -> (\o -> TestBox o ws) <$> genOrientation)
        <> [Gen.subterm genTestWidget TestScrolledWindow]
    )
  where
    -- In lack of `subtermN` (https://github.com/hedgehogqa/haskell-hedgehog/issues/119), we use this terrible hack:
    subwidgets :: ([TestWidget] -> Gen TestWidget) -> [Gen TestWidget]
    subwidgets f =
      [ f [],
        Gen.subtermM genTestWidget (\w -> f [w]),
        Gen.subtermM2 genTestWidget genTestWidget (\w1 w2 -> f [w1, w2]),
        Gen.subtermM3 genTestWidget genTestWidget genTestWidget (\w1 w2 w3 -> f [w1, w2, w3])
      ]

genOrientation :: Gen Gtk.Orientation
genOrientation = Gen.choice [pure Gtk.OrientationVertical, pure Gtk.OrientationHorizontal]

genLabel :: Gen TestWidget
genLabel = do
  TestLabel <$> Gen.text (Range.linear 0 10) Gen.unicode

genButton :: Gen TestWidget
genButton = do
  TestButton <$> Gen.text (Range.linear 0 10) Gen.unicode
