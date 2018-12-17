{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Paned where

import           Control.Monad                 (void)

import           GI.Gtk                        (Label (..), Paned (..),
                                                Window (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

data State = Initial

data Event = Closed

view' :: State -> AppView Event
view' s =
  bin Window [#title := "Hello", on #deleteEvent (const (True, Closed)), #widthRequest := 400, #heightRequest := 300] $
    case s of
      Initial  ->
        container Paned [#wideHandle := True] $ do
          -- NOTE: There can only be two child panes in a Paned
          -- widget. Any additional will be ignored.
          pane (Resize True) (Shrink True) $ widget Label [#label := "Left"]
          pane (Resize True) (Shrink True) $ widget Label [#label := "Right"]

update' :: State -> Event -> Transition State Event
update' _ Closed      = Exit

main :: IO ()
main = void $ run App
  { view         = view'
  , update       = update'
  , inputs       = []
  , initialState = Initial
  }
