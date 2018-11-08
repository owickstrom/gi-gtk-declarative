{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module AddBoxes where

import qualified Data.Text                     as Text

import           GI.Gtk                         ( Box(..)
                                                , Button(..)
                                                , Label(..)
                                                , Orientation(..)
                                                , PolicyType(..)
                                                , Window(..)
                                                , ScrolledWindow(..)
                                                )
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

data Event = AddLeft | AddRight | Closed

data State = State { lefts :: [Int], rights :: [Int], next :: Int }

addBoxesView :: State -> AppView Event
addBoxesView State {..} =
  bin Window [#title := "AddBoxes", on #deleteEvent (const (True, Closed)), #widthRequest := 400, #heightRequest := 300]
    $ bin
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
  renderLane onClick children = boxChild True True 10 $
    container Box [] $ do
      boxChild False False 10 $ do
        widget Button [#label := "Add", on #clicked onClick]
      (mapM_ (boxChild False False 5 . renderChild) children)
  renderChild :: Int -> Widget Event
  renderChild n = widget Label [#label := Text.pack (show n)]

update' :: State -> Event -> Transition State Event
update' state@State {..} AddLeft = Transition
  state { lefts = lefts ++ [next], next = succ next }
  (return Nothing)
update' state@State {..} AddRight = Transition
  state { rights = rights ++ [next], next = succ next }
  (return Nothing)
update' _ Closed = Exit

main :: IO ()
main = run App
  { view         = addBoxesView
  , update       = update'
  , inputs       = []
  , initialState = State [1] [2] 3
  }
