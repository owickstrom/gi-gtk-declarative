{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module GI.Gtk.Declarative.TestWidget where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Int                           ( Int32 )
import           Data.Text                          ( Text )
import           Data.Traversable                   ( for )
import           Data.List                          ( sortOn )
import           Data.Vector                        ( Vector )
import qualified Data.Vector                       as Vector
import           Data.Void
import qualified GI.Gtk                            as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Container.Grid  ( GridChild (..), GridChildProperties (..) )
import           GI.Gtk.Declarative.Container.Grid as Grid
import           GI.Gtk.Declarative.EventSource
import           Hedgehog                    hiding ( label )
import qualified Hedgehog.Gen                      as Gen
import qualified Hedgehog.Range                    as Range
import           Prelude

-- | GTK widgets cannot (in any practical, generic sense) be compared and shown
-- in tests, so we represent widgets in property tests using this data
-- structure. We convert between this representation, declarative widgets, and
-- instantiated GTK widgets.
data TestWidget
  = TestButton Text (Maybe Bool)
  | TestCustomWidget (Maybe Text)
  | TestScrolledWindow (Maybe Gtk.PolicyType) TestWidget
  | TestBox (Maybe Gtk.Orientation) [TestBoxChild]
  | TestGrid [TestGridChild]
  deriving (Eq, Show)

data TestBoxChild = TestBoxChild BoxChildProperties TestWidget
  deriving (Eq, Show)

data TestGridChild = TestGridChild GridChildProperties TestWidget
  deriving (Eq, Show)

isNested :: TestWidget -> Bool
isNested = \case
  TestButton{}                  -> False
  TestCustomWidget{}            -> False
  TestScrolledWindow _ _        -> True
  TestBox            _ children -> not (null children)
  TestGrid children             -> not (null children)

class HasGtkDefaults a where
  setDefaults :: a -> a

instance HasGtkDefaults TestWidget where
  setDefaults = \case
    TestButton label useUnderline ->
      TestButton label (useUnderline <|> Just False)
    TestCustomWidget fontName -> TestCustomWidget (fontName <|> Just "Sans 12")
    TestScrolledWindow policy child -> TestScrolledWindow
      (policy <|> Just Gtk.PolicyTypeAutomatic)
      (setDefaults child)
    TestBox orientation children -> TestBox
      (orientation <|> Just Gtk.OrientationHorizontal)
      (map setDefaults children)
    TestGrid children ->
      TestGrid (map setDefaults children)

instance HasGtkDefaults TestBoxChild where
  setDefaults = \case
    TestBoxChild props child -> TestBoxChild props (setDefaults child)

instance HasGtkDefaults TestGridChild where
  setDefaults = \case
    TestGridChild props child -> TestGridChild props (setDefaults child)

onlyJusts :: Vector (Maybe a) -> Vector a
onlyJusts = Vector.concatMap (maybe Vector.empty Vector.singleton)

toTestWidget :: TestWidget -> Widget Void
toTestWidget = \case
  TestCustomWidget fontName -> Widget (CustomWidget { .. })
   where
    customParams     = ()
    customAttributes = case fontName of
      Just t  -> [#fontName := t]
      Nothing -> []
    customWidget = Gtk.FontButton
    customCreate () = do
      btn <- Gtk.new Gtk.FontButton []
      return (btn, ())
    customPatch :: () -> () -> () -> CustomPatch Gtk.FontButton ()
    customPatch _ () () = CustomKeep
    customSubscribe
      :: () -> () -> Gtk.FontButton -> (Void -> IO ()) -> IO Subscription
    customSubscribe () () _lbl _cb = do
      return (fromCancellation (pure ()))
  TestButton label useUnderline -> widget
    Gtk.Button
    (onlyJusts [Just (#label := label), (#useUnderline :=) <$> useUnderline])
  TestScrolledWindow policy child -> bin
    Gtk.ScrolledWindow
    (onlyJusts [(#vscrollbarPolicy :=) <$> policy])
    (toTestWidget child)
  TestBox orientation children -> container
    Gtk.Box
    (onlyJusts [(#orientation :=) <$> orientation])
    (Vector.map
      (\(TestBoxChild props child) -> BoxChild props (toTestWidget child))
      (Vector.fromList children)
    )
  TestGrid children -> container
    Gtk.Grid
    []
    (Vector.map
      (\(TestGridChild props child) -> GridChild props (toTestWidget child))
      (Vector.fromList children)
    )

fromGtkWidget :: (MonadIO m) => Gtk.Widget -> m (Either Text TestWidget)
fromGtkWidget = runExceptT . go
 where
  go :: (MonadIO m) => Gtk.Widget -> ExceptT Text m TestWidget
  go w = do
    name <- #getName w
    case name of
      "GtkButton" -> withCast
        w
        Gtk.Button
        (\btn ->
          TestButton
            <$> Gtk.get btn #label
            <*> (Just <$> Gtk.get btn #useUnderline)
        )
      "GtkFontButton" -> withCast
        w
        Gtk.FontButton
        (\btn -> TestCustomWidget . Just <$> Gtk.get btn #fontName)
      "GtkScrolledWindow" -> withCast w Gtk.ScrolledWindow $ \win -> do
        w' <-
          #getChild win
            >>= maybe (throwError "No viewport in scrolled window") pure
        vscrollbarPolicy <- Just <$> Gtk.get win #vscrollbarPolicy
        withCast w' Gtk.Viewport $ \viewport -> do
          child <-
            #getChild viewport
              >>= maybe (throwError "No child in scrolled window") pure
          TestScrolledWindow vscrollbarPolicy <$> go child
      "GtkBox" -> withCast w Gtk.Box $ \box -> do
        childGtkWidgets <- #getChildren box
        boxChildProps   <- for childGtkWidgets $ \childGtkWidget -> do
          (expand, fill, padding, _) <- #queryChildPacking box childGtkWidget
          pure (BoxChildProperties expand fill padding)
        childWidgets <- traverse go childGtkWidgets
        orientation  <- Just <$> Gtk.get box #orientation
        pure
          (TestBox orientation (zipWith TestBoxChild boxChildProps childWidgets)
          )
      "GtkGrid" -> withCast w Gtk.Grid $ \grid -> do
        childGtkWidgets <- #getChildren grid
        gridChildren    <- for childGtkWidgets $ \childGtkWidget -> do
          let prop = getGridChildProp grid childGtkWidget
          height     <- prop "height"
          width      <- prop "width"
          leftAttach <- prop "left-attach"
          topAttach  <- prop "top-attach"
          child      <- go childGtkWidget
          pure (TestGridChild GridChildProperties {..} child)
        -- the order of the children is not maintained by the Grid, so we
        -- need to sort here to allow accurate comparisons of the children
        let sortedChildren =
              sortOn (\(TestGridChild p _) -> topAttach p) gridChildren
        pure (TestGrid sortedChildren)
      _ -> throwError ("Unsupported TestWidget: " <> name)
  withCast
    :: (MonadIO m, Gtk.GObject w, Gtk.GObject w')
    => w
    -> (Gtk.ManagedPtr w' -> w')
    -> (w' -> ExceptT Text m a)
    -> ExceptT Text m a
  withCast w ctor f = liftIO (Gtk.castTo ctor w) >>= \case
    Just w' -> f w'
    Nothing -> throwError "Failed to cast widget"

getGridChildProp
  :: (MonadIO m) => Gtk.Grid -> Gtk.Widget -> Text -> ExceptT Text m Int32
getGridChildProp grid child prop = do
  gValue <- liftIO (Gtk.toGValue (0 :: Int32))
  Gtk.containerChildGetProperty grid child prop gValue
  liftIO (Gtk.fromGValue gValue)

-- * Generators

genTestWidget :: Gen TestWidget
genTestWidget = Gen.frequency
  (map (3, ) leaves <> pure
    ( 2
    , Gen.recursive
      Gen.choice
      leaves
      (  subwidgets genTestBoxFrom
      <> subwidgets getTestGridFrom
      <> [ Gen.subtermM
             genTestWidget
             (\c -> TestScrolledWindow <$> Gen.maybe genPolicyType <*> pure c)
         ]
      )
    )
  )
 where
  leaves = [genCustomWidget, genButton]
  -- In lack of `subtermN` (https://github.com/hedgehogqa/haskell-hedgehog/issues/119), we use this terrible hack:
  subwidgets :: ([TestWidget] -> Gen TestWidget) -> [Gen TestWidget]
  subwidgets f =
    [ f []
    , Gen.subtermM genTestWidget (\w -> f [w])
    , Gen.subtermM2 genTestWidget genTestWidget (\w1 w2 -> f [w1, w2])
    , Gen.subtermM3 genTestWidget
                    genTestWidget
                    genTestWidget
                    (\w1 w2 w3 -> f [w1, w2, w3])
    ]
  genTestBoxFrom ws = do
    children <- for ws $ \w -> do
      props <- genBoxChildProperties
      pure (TestBoxChild props w)
    o <- Gen.maybe genOrientation
    pure (TestBox o children)
  getTestGridFrom ws = do
    children <- for (zip [0 ..] ws) $ \(i, w) -> do
      props <- genGridChildProperties i
      pure (TestGridChild props w)
    pure (TestGrid children)

genOrientation :: Gen Gtk.Orientation
genOrientation =
  Gen.choice [pure Gtk.OrientationVertical, pure Gtk.OrientationHorizontal]

genPolicyType :: Gen Gtk.PolicyType
genPolicyType = Gen.choice
  (map
    pure
    [ Gtk.PolicyTypeAlways
    , Gtk.PolicyTypeAutomatic
    , Gtk.PolicyTypeExternal
    , Gtk.PolicyTypeNever
    ]
  )

genBoxChildProperties :: Gen BoxChildProperties
genBoxChildProperties =
  BoxChildProperties <$> Gen.bool <*> Gen.bool <*> Gen.word32
    (Range.linear 0 10)

genGridChildProperties :: Int32 -> Gen GridChildProperties
genGridChildProperties rowN = do
  width      <- Gen.int32 (Range.linear 1 10)
  height     <- Gen.int32 (Range.linear 1 5)
  leftAttach <- Gen.int32 (Range.linear 0 10)
  topAttach  <- Gen.int32 (Range.constant (rowN * 5) (rowN * 5 + 5 - height))
  pure GridChildProperties {..}

genCustomWidget :: Gen TestWidget
genCustomWidget = do
  TestCustomWidget <$> Gen.maybe (Gen.choice [pure "Sans 10"])

genButton :: Gen TestWidget
genButton = do
  TestButton <$> Gen.text (Range.linear 0 10) Gen.unicode <*> Gen.maybe Gen.bool
