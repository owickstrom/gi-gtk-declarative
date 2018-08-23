{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

module Hello where

import           Data.Function ((&))
import           Data.Text                                ( Text )
import           GHC.Exts
import           Pipes
import qualified Pipes.Extras                         as Pipes

import           GI.Gtk.Declarative                hiding ( main )
import           GI.Gtk.Declarative.App.Simple

data Model = Initial | Running Text Bool

data Event = Greet Text

helloView :: Model -> Markup Event
helloView Initial               = node Label [#label := "Nothing here yet."]
helloView (Running who flipped) = container Box [] $ op
  [ BoxChild True
             True
             0
             (node Label [#label := "Stuff is happening."])
  , BoxChild True True 0 (node Label [#label := who])
  ]
 where
  op :: [BoxChild Event] -> Children BoxChild Event
  op = fromList . if flipped then reverse else id

update' :: Model -> Event -> (Model, IO (Maybe Event))
update' Initial (Greet who) = (Running who False, return Nothing)
update' (Running _ flipped) (Greet who) =
  (Running who (not flipped), return Nothing)

main :: IO ()
main = run "Hello" (Just (640, 480)) app Initial
  where
    greetings =
      cycle ["Joe", "Mike"]
      & map (\n -> (Greet ("Hello, " <> n)))
      & Pipes.each
      & (>-> Pipes.delay 1.0)

    app = App {view = helloView, update = update', inputs = [greetings]}


