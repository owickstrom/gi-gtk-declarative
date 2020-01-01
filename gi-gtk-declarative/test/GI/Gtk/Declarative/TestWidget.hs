{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module GI.Gtk.Declarative.TestWidget where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable (for)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative
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
  = TestButton Text (Maybe Bool)
  | TestLabel (Maybe Text)
  | TestScrolledWindow (Maybe Gtk.PolicyType) TestWidget
  | TestBox (Maybe Gtk.Orientation) [TestBoxChild]
  deriving (Eq, Show)

data TestBoxChild = TestBoxChild BoxChildProperties TestWidget
  deriving (Eq, Show)

isNested :: TestWidget -> Bool
isNested = \case
  TestButton {} -> False
  TestLabel {} -> False
  TestScrolledWindow _ _ -> True
  TestBox _ children -> not (null children)

class HasGtkDefaults a where
  setDefaults :: a -> a

instance HasGtkDefaults TestWidget where
  setDefaults = \case
    TestButton label useUnderline -> TestButton label (useUnderline <|> Just False)
    TestLabel label -> TestLabel (label <|> Just mempty)
    TestScrolledWindow policy child -> TestScrolledWindow (policy <|> Just Gtk.PolicyTypeAutomatic) (setDefaults child)
    TestBox orientation children -> TestBox (orientation <|> Just Gtk.OrientationHorizontal) (map setDefaults children)

instance HasGtkDefaults TestBoxChild where
  setDefaults = \case
    TestBoxChild props child -> TestBoxChild props (setDefaults child)

onlyJusts :: Vector (Maybe a) -> Vector a
onlyJusts = Vector.concatMap (maybe Vector.empty Vector.singleton)

toTestWidget :: TestWidget -> Widget Void
toTestWidget = \case
  TestLabel label -> widget Gtk.Label (onlyJusts [(#label :=) <$> label])
  TestButton label useUnderline -> widget Gtk.Button (onlyJusts [Just (#label := label), (#useUnderline :=) <$> useUnderline])
  TestScrolledWindow policy child -> bin Gtk.ScrolledWindow (onlyJusts [(#vscrollbarPolicy :=) <$> policy]) (toTestWidget child)
  TestBox orientation children ->
    container
      Gtk.Box
      (onlyJusts [(#orientation :=) <$> orientation])
      ( Vector.map
          (\(TestBoxChild props child) -> BoxChild props (toTestWidget child))
          (Vector.fromList children)
      )

fromGtkWidget :: (MonadIO m) => Gtk.Widget -> m (Either Text TestWidget)
fromGtkWidget = runExceptT . go
  where
    go :: (MonadIO m) => Gtk.Widget -> ExceptT Text m TestWidget
    go w = do
      name <- #getName w
      case name of
        "GtkButton" -> withCast w Gtk.Button (\btn -> TestButton <$> Gtk.get btn #label <*> (Just <$> Gtk.get btn #useUnderline))
        "GtkLabel" -> withCast w Gtk.Label (\lbl -> TestLabel . Just <$> Gtk.get lbl #label)
        "GtkScrolledWindow" -> withCast w Gtk.ScrolledWindow $ \win -> do
          w' <- #getChild win >>= maybe (throwError "No viewport in scrolled window") pure
          vscrollbarPolicy <- Just <$> Gtk.get win #vscrollbarPolicy
          withCast w' Gtk.Viewport $ \viewport -> do
            child <- #getChild viewport >>= maybe (throwError "No child in scrolled window") pure
            TestScrolledWindow vscrollbarPolicy <$> go child
        "GtkBox" -> withCast w Gtk.Box $ \box -> do
          childGtkWidgets <- #getChildren box
          boxChildProps <- for childGtkWidgets $ \childGtkWidget -> do
            (expand, fill, padding, _) <- #queryChildPacking box childGtkWidget
            pure (BoxChildProperties expand fill padding)
          childWidgets <- traverse go childGtkWidgets
          orientation <- Just <$> Gtk.get box #orientation
          pure (TestBox orientation (zipWith TestBoxChild boxChildProps childWidgets))
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
  Gen.frequency
    (map (2,) leaves <>
      pure ( 1,
        Gen.recursive
          Gen.choice
          leaves
          ( subwidgets genTestBoxFrom
              <> [Gen.subtermM genTestWidget (\c -> TestScrolledWindow <$> Gen.maybe genPolicyType <*> pure c)]
          )
      )
    )
  where
    leaves = [genLabel, genButton]
    -- In lack of `subtermN` (https://github.com/hedgehogqa/haskell-hedgehog/issues/119), we use this terrible hack:
    subwidgets :: ([TestWidget] -> Gen TestWidget) -> [Gen TestWidget]
    subwidgets f =
      [ f [],
        Gen.subtermM genTestWidget (\w -> f [w]),
        Gen.subtermM2 genTestWidget genTestWidget (\w1 w2 -> f [w1, w2]),
        Gen.subtermM3 genTestWidget genTestWidget genTestWidget (\w1 w2 w3 -> f [w1, w2, w3])
      ]
    genTestBoxFrom ws = do
      children <- for ws $ \w -> do
        props <- genBoxChildProperties
        pure (TestBoxChild props w)
      o <- Gen.maybe genOrientation
      pure (TestBox o children)

genOrientation :: Gen Gtk.Orientation
genOrientation = Gen.choice [pure Gtk.OrientationVertical, pure Gtk.OrientationHorizontal]

genPolicyType :: Gen Gtk.PolicyType
genPolicyType =
  Gen.choice
    ( map
        pure
        [ Gtk.PolicyTypeAlways,
          Gtk.PolicyTypeAutomatic,
          Gtk.PolicyTypeExternal,
          Gtk.PolicyTypeNever
        ]
    )

genBoxChildProperties :: Gen BoxChildProperties
genBoxChildProperties =
  BoxChildProperties
    <$> Gen.bool
    <*> Gen.bool
    <*> Gen.word32 (Range.linear 0 10)

genLabel :: Gen TestWidget
genLabel = do
  TestLabel <$> Gen.maybe (Gen.text (Range.linear 0 10) Gen.unicode)

genButton :: Gen TestWidget
genButton =
  do
    TestButton <$> Gen.text (Range.linear 0 10) Gen.unicode
      <*> Gen.maybe Gen.bool
