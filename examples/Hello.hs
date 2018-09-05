{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Hello where

import           Data.Function                 ((&))
import           Data.Text                     (Text)
import           Pipes
import qualified Pipes.Extras                  as Pipes

import           GI.Gtk                        (Label (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

data State = Initial | Greeting Text

data Event = Greet Text

view' :: State -> Widget Event
view' Initial        = widget Label [#label := "Nothing here yet."]
view' (Greeting who) = widget Label [#label := who]

update' :: State -> Event -> Transition State Event
update' _ (Greet who) = Transition (Greeting who) (return Nothing)

main :: IO ()
main = run
  "Hello"
  (Just (640, 480))
  App
    { view         = view'
    , update       = update'
    , inputs       = [greetings]
    , initialState = Initial
    }
 where
  greetings =
    cycle ["Joe", "Mike"]
      & map (\n -> (Greet ("Hello, " <> n)))
      & Pipes.each
      & (>-> Pipes.delay 1.0)
