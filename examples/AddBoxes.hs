{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module AddBoxes where

import           Control.Monad                 (void)
import qualified Data.Text                     as Text

import           GI.Gtk                        (Box (..), Button (..),
                                                Label (..), Orientation (..),
                                                PolicyType (..),
                                                ScrolledWindow (..),
                                                Window (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

data Event = AddLeft | AddRight | Closed

data State = State { lefts :: [Int], rights :: [Int], next :: Int }

addBoxesView :: State -> AppView Window Event
addBoxesView State {..} =
  bin
      Window
      [ #title := "AddBoxes"
      , on #deleteEvent (const (True, Closed))
      , #widthRequest := 400
      , #heightRequest := 300
      ]
    $ bin
        ScrolledWindow
        [ #hscrollbarPolicy := PolicyTypeAutomatic
        , #vscrollbarPolicy := PolicyTypeNever
        ]
    $ container Box
                [#orientation := OrientationVertical]
                [renderLane AddLeft lefts, renderLane AddRight rights]
 where
  renderLane :: Event -> [Int] -> BoxChild Event
  renderLane onClick children =
    BoxChild defaultBoxChildProperties { padding = 10 } $ container
      Box
      []
      ( BoxChild defaultBoxChildProperties { padding = 10 } btn
      : map
          (BoxChild defaultBoxChildProperties { padding = 5 } . renderChild)
          children
      )
    where btn = widget Button [#label := "Add", on #clicked onClick]
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
main = void $ run App
  { view         = addBoxesView
  , update       = update'
  , inputs       = []
  , initialState = State [1] [2] 3
  }
