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
                                                )
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

data State = State { greetings :: [Text] }

data Event = Greet Text

view' :: State -> AppView Event
view' State {..} =
  bin Window [#title := "ListBox"]
    $ container ListBox []
    $ forM_ greetings
    $ \name ->
        bin ListBoxRow [#activatable := False, #selectable := False]
          $ widget Label [#label := name]

update' :: State -> Event -> Transition State Event
update' State {..} (Greet who) =
  Transition State { greetings = greetings <> [who] } (pure Nothing)

main :: IO ()
main = run App
  { view         = view'
  , update       = update'
  , inputs       = [greetings]
  , initialState = State []
  }
 where
  greetings =
    cycle ["Joe", "Mike"]
      & map (\n -> (Greet ("Hello, " <> n)))
      & Pipes.each
      & (>-> Pipes.delay 1.0)
      & (>-> Pipes.delay 1.0)
