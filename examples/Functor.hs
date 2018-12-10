{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Functor where

import           Data.Functor                  (($>))
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Control.Monad                 (void)

import           GI.Gtk                        (Box (..), Button (..),
                                                Label (..), Orientation (..),
                                                Window (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

data ButtonEvent = ButtonClicked

clickyButton :: Text -> Widget ButtonEvent
clickyButton label = widget Button [#label := label, on #clicked ButtonClicked]

data State = State { count :: Integer }

data Event = Incr | Decr | Closed

incrDecrView :: State -> AppView Event
incrDecrView State {..} =
  bin
      Window
      [ #title := "Functor"
      , on #deleteEvent (const (True, Closed))
      , #widthRequest := 400
      , #heightRequest := 300
      ]
    $ container Box [#orientation := OrientationVertical]
    $ do
        boxChild True True 0 $ widget Label [#label := Text.pack (show count)]
        boxChild False False 0
          $ container Box [#orientation := OrientationHorizontal]
          $ do
              boxChild True True 0 $ clickyButton "-1" $> Decr
              boxChild True True 0 $ clickyButton "+1" $> Incr

update' :: State -> Event -> Transition State Event
update' State {..} Incr   = Transition (State (count + 1)) (return Nothing)
update' State {..} Decr   = Transition (State (count - 1)) (return Nothing)
update' _          Closed = Exit

main :: IO ()
main = void $ run App
  { view         = incrDecrView
  , update       = update'
  , inputs       = []
  , initialState = State 0
  }
