{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module AddBoxes where

import           Control.Concurrent
import           Control.Monad
import qualified Data.Text                     as Text

import           GI.Gtk.Declarative            hiding (main)
import qualified GI.Gtk.Declarative            as Gtk
import           GI.Gtk.Declarative.App.Simple


data Event = AddLeft | AddRight

data Model = Model { lefts :: [Int], rights :: [Int], next :: Int }

addBoxesView :: Model -> Widget Event
addBoxesView Model {..} = container
  ScrolledWindow
  [ #hscrollbarPolicy := PolicyTypeAutomatic
  , #vscrollbarPolicy := PolicyTypeNever
  ]
  windowContents
 where
  windowContents :: Widget Event
  windowContents = container Box [#orientation := OrientationVertical] $ do
    renderLane AddLeft  lefts
    renderLane AddRight rights
  renderLane :: Event -> [Int] -> MarkupOf BoxChild Event ()
  renderLane onClick children = boxChild True True 10 $ do
    container Box [] $ do
      boxChild False False 10 $ do
        node Button [#label := "Add", on #clicked onClick]
      mapM_ renderChild children
  renderChild :: Int -> MarkupOf BoxChild Event ()
  renderChild n =
    boxChild True True 0 $ node Label [#label := Text.pack ("Box " <> show n)]

update' :: Model -> Event -> (Model, IO (Maybe Event))
update' model@Model {..} AddLeft =
  (model { lefts = lefts ++ [next], next = succ next }, return Nothing)
update' model@Model {..} AddRight =
  (model { rights = rights ++ [next], next = succ next }, return Nothing)

main :: IO ()
main = do
  void $ Gtk.init Nothing
  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  void (Gtk.onWidgetDestroy window mainQuit)

  Gtk.windowSetTitle window "AddBoxes"
  Gtk.windowResize window 640 480

  let app = App {view = addBoxesView, update = update', inputs = []}

  void . forkIO $ runInWindow window app (Model [1] [2] 3)
  -- Let's do it!
  Gtk.main
