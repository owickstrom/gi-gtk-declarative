{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Functor where

import           Data.Functor                  (($>))
import           Data.Text                     (Text)
import qualified Data.Text                     as Text

import           GI.Gtk                        (Box (..), Button (..),
                                                Label (..), Orientation (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

data ButtonEvent = ButtonClicked

clickyButton :: Text -> Widget ButtonEvent
clickyButton label =
  widget Button [ #label := label, on #clicked ButtonClicked ]

data State = State { count :: Integer }

data Event = Incr | Decr

incrDecrView :: State -> Widget Event
incrDecrView State {..} =
  container Box [ #orientation := OrientationVertical ] $ do
    boxChild True True 0 $
      widget Label [ #label := Text.pack (show count) ]
    boxChild False False 0 $
      container Box [ #orientation := OrientationHorizontal ] $ do
        boxChild True True 0 $ clickyButton "-1" $> Decr
        boxChild True True 0 $ clickyButton "+1" $> Incr

update' :: State -> Event -> Continuation State Event
update' State{..} Incr = Continue (State (count + 1)) (return Nothing)
update' State{..} Decr = Continue (State (count - 1)) (return Nothing)

main :: IO ()
main =
  run
    "Hello"
    (Just (640, 480))
    App
    {view = incrDecrView, update = update', inputs = [], initialState = State 0}
