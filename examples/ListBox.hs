{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ListBox where

import           Control.Monad                 (void)
import           Data.Function                 ((&))
import           Data.Functor                  ((<&>))
import           Data.Text                     (Text)
import           Pipes
import qualified Pipes.Extras                  as Pipes

import           GI.Gtk                        (Label (..), ListBox (..),
                                                ListBoxRow (..), Window (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

newtype State = State { greetings :: [Text] }

data Event = Greet Text | Closed

view' :: State -> AppView Window Event
view' State {..} =
  bin
      Window
      [ #title := "ListBox"
      , on #deleteEvent (const (True, Closed))
      , #widthRequest := 400
      , #heightRequest := 300
      ]
    $ container ListBox []
    $ greetings <&> \name ->
        bin ListBoxRow [#activatable := False, #selectable := False]
          $ widget Label [#label := name]

update' :: State -> Event -> Transition State Event
update' State {..} (Greet who) =
  Transition State {greetings = greetings <> [who]} (pure Nothing)
update' _ Closed = Exit

main :: IO ()
main = void $ run App
  { view         = view'
  , update       = update'
  , inputs       = [greetings]
  , initialState = State []
  }
 where
  greetings =
    cycle ["Joe", "Mike"]
      & map (\n -> Greet ("Hello, " <> n))
      & Pipes.each
      & (>-> Pipes.delay 1.0)
      & (>-> Pipes.delay 1.0)
