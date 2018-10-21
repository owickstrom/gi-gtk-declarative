{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ListBox where

import           Control.Monad                  ( forM_ )
import           Data.Function                  ( (&) )
import           Data.Text                      ( Text )
import           Pipes
import qualified Pipes.Extras                  as Pipes

import           GI.Gtk                         ( Label(..)
                                                , ListBox(..)
                                                , Window(..)
                                                , ListBoxRow(..)
                                                , Box(..)
                                                )
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

import Debug.Trace

data State = State { greetings :: [Text] }

data Event = Greet Text | Closed

view' :: State -> AppView Event
view' State {..} =
  bin Window [#title := "ListBox", on #deleteEvent (const (True, Closed)), #widthRequest := 400, #heightRequest := 300]
    $ container ListBox []
    -- $ container Box []
    $ forM_ greetings
    $ \name ->
        bin ListBoxRow [#activatable := False, #selectable := False]
        -- boxChild False False 2
          $ widget Label [#label := name]

update' :: State -> Event -> Transition State Event
update' State {..} (Greet who) =
  Transition State { greetings = greetings <> [who] } (pure Nothing)
update' _ Closed = Exit

main :: IO ()
main = run App
  { view         = view'
  , update       = update'
  , inputs       = [greetings]
  , initialState = State ["Nothing yet."]
  }
 where
  greetings =
    cycle ["Joe", "Mike"]
      & map (\n -> (Greet ("Hello, " <> n)))
      & Pipes.each
      & (>-> Pipes.delay 1.0)
      & (>-> Pipes.delay 1.0)
