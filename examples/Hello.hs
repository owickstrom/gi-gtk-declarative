{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

module Hello where

import           Data.Function ((&))
import           Data.Text                                ( Text )
import           Pipes
import qualified Pipes.Extras                         as Pipes

import           GI.Gtk                        (Label (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

data State = Initial | Greeting Text

data Event = Greet Text

helloView :: State -> Widget Event
helloView Initial       = widget Label [#label := "Nothing here yet."]
helloView (Greeting who) = widget Label [#label := who]

update' :: State -> Event -> Continuation State Event
update' _ (Greet who) = Continue (Greeting who) (return Nothing)

main :: IO ()
main =
  run
    "Hello"
    (Just (640, 480))
    App
    { view = helloView
    , update = update'
    , inputs = [greetings]
    , initialState = Initial
    }
  where
    greetings =
      cycle ["Joe", "Mike"]
      & map (\n -> (Greet ("Hello, " <> n)))
      & Pipes.each
      & (>-> Pipes.delay 1.0)
