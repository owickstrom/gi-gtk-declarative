{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ListBox where

import           Data.Function                 ((&))
import           Data.Text                     (Text)
import           Pipes
import qualified Pipes.Extras                  as Pipes

import           GI.Gtk                        (Label (..), ListBox (..), ListBoxRow(..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

data Model = Model { greetings :: [Text] }

data Event = Greet Text

view' :: Model -> Widget Event
view' Model {..} = container ListBox [] (mapM_ renderGreeting greetings)
 where
  renderGreeting :: Text -> MarkupOf (Bin ListBoxRow Widget) Event ()
  renderGreeting name = do
    bin ListBoxRow [] $
      widget Label [ #label := "The following is in a ListBoxRow:" ]
    bin ListBoxRow [] $
      widget Label [ #label := name ]

update' :: Model -> Event -> (Model, IO (Maybe Event))
update' Model{..} (Greet who) = (Model {greetings = greetings <> [who]}, return Nothing)

main :: IO ()
main = run "Hello" (Just (640, 480)) app (Model [])
  where
    greetings =
      cycle ["Joe", "Mike"]
      & map (\n -> (Greet ("Hello, " <> n)))
      & Pipes.each
      & (>-> Pipes.delay 1.0)

    app = App {view = view', update = update', inputs = [greetings]}
