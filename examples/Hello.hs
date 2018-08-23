{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

module Hello where

import           Data.Function ((&))
import           Data.Text                                ( Text )
import           Pipes
import qualified Pipes.Extras                         as Pipes

import           GI.Gtk.Declarative                hiding ( main )
import           GI.Gtk.Declarative.App.Simple

data Model = Initial | Greeting Text

data Event = Greet Text

helloView :: Model -> Widget Event
helloView Initial       = node Label [#label := "Nothing here yet."]
helloView (Greeting who) = node Label [#label := who]

update' :: Model -> Event -> (Model, IO (Maybe Event))
update' _ (Greet who) = (Greeting who, return Nothing)

main :: IO ()
main = run "Hello" (Just (640, 480)) app Initial
  where
    greetings =
      cycle ["Joe", "Mike"]
      & map (\n -> (Greet ("Hello, " <> n)))
      & Pipes.each
      & (>-> Pipes.delay 1.0)

    app = App {view = helloView, update = update', inputs = [greetings]}


