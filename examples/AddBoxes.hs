{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module AddBoxes where

import           Control.Concurrent
import           Control.Monad
import qualified Data.Text                     as Text

import           GI.Gtk.Declarative                hiding ( main )
import qualified GI.Gtk.Declarative            as Gtk
import           GI.Gtk.Declarative.App.Simple


type Event = Either () ()

data Model = Model { lefts :: [Int], rights :: [Int], next :: Int }

addBoxesView :: Model -> Markup Event
addBoxesView Model {..} = container
  ScrolledWindow
  [ #hscrollbarPolicy := PolicyTypeAutomatic
  , #vscrollbarPolicy := PolicyTypeNever
  ]
  (container Box
             [#orientation := OrientationVertical]
             [renderLane (Left ()) lefts, renderLane (Right ()) rights]
  )
 where
  renderLane :: Event -> [Int] -> BoxChild Event
  renderLane onClick children = BoxChild
    True
    True
    10
    (container
      Box
      []
      ( BoxChild False
                  False
                  10
                  (node Button [#label := "Add", on #clicked onClick])
      : map renderChild children
      )
    )
  renderChild :: Int -> BoxChild Event
  renderChild n =
    BoxChild True True 0 (node Label [#label := Text.pack ("Box " <> show n)])

update' :: Model -> Event -> (Model, IO (Maybe Event))
update' model@Model {..} (Left ()) =
  (model { lefts = lefts ++ [next], next = succ next }, return Nothing)
update' model@Model {..} (Right ()) =
  (model { rights = rights ++ [next], next = succ next }, return Nothing)

main :: IO ()
main = do
  void $ Gtk.init Nothing
  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  void (Gtk.onWidgetDestroy window mainQuit)

  Gtk.windowSetTitle window "AddBoxes"
  Gtk.windowResize window 640 480

  let app = App {view = addBoxesView, update = update', inputs = [] }

  void . forkIO $ runInWindow window app (Model [1] [2] 3)

  -- Let's do it!
  Gtk.main
