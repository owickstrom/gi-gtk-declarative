{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module AddBoxes where

import qualified Data.Text                     as Text

import           GI.Gtk                        (Box (..), Button (..),
                                                Label (..), Orientation (..),
                                                PolicyType (..),
                                                ScrolledWindow (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple


data Event = AddLeft | AddRight

data State = State { lefts :: [Int], rights :: [Int], next :: Int }

addBoxesView :: State -> Widget Event
addBoxesView State {..} =
  bin
      ScrolledWindow
      [ #hscrollbarPolicy := PolicyTypeAutomatic
      , #vscrollbarPolicy := PolicyTypeNever
      ]
    $ container Box [#orientation := OrientationVertical]
    $ do
         renderLane AddLeft  lefts
         renderLane AddRight rights
 where
  renderLane :: Event -> [Int] -> MarkupOf BoxChild Event ()
  renderLane onClick children = boxChild True True 10 $ do
    container Box [] $ do
      boxChild False False 10 $ do
        widget Button [#label := "Add", on #clicked onClick]
      (mapM_ (boxChild False False 0 . renderChild) children)
  renderChild :: Int -> Widget Event
  renderChild n = widget Label [#label := Text.pack ("Box " <> show n)]

update' :: State -> Event -> Continuation State Event
update' state@State {..} AddLeft =
  Continue state {lefts = lefts ++ [next], next = succ next} (return Nothing)
update' state@State {..} AddRight =
  Continue state {rights = rights ++ [next], next = succ next} (return Nothing)

main :: IO ()
main =
  let app = App {view = addBoxesView, update = update', inputs = []}
  in  run "AddBoxes" (Just (640, 480)) app (State [1] [2] 3)
