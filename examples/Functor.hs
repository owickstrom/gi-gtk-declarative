{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Functor where

import           Control.Monad                 (void)
import           Data.Functor                  (($>))
import           Data.Text                     (Text)
import qualified Data.Text                     as Text

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

incrDecrView :: State -> AppView Window Event
incrDecrView State {..} =
  bin
      Window
      [ #title := "Functor"
      , on #deleteEvent (const (True, Closed))
      , #widthRequest := 400
      , #heightRequest := 300
      ]
    $ container
        Box
        [#orientation := OrientationVertical]
        [ expandingChild $ widget Label [#label := Text.pack (show count)]
        , BoxChild defaultBoxChildProperties $ container
          Box
          [#orientation := OrientationHorizontal]
          [ expandingChild $ clickyButton "-1" $> Decr
          , expandingChild $ clickyButton "+1" $> Incr
          ]
        ]
 where
  expandingChild =
    BoxChild defaultBoxChildProperties { expand = True, fill = True }

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
