{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Notebook where

import           Control.Concurrent            (threadDelay)
import           Control.Monad                 (void)
import           Data.Text                     (pack)
import           GI.Gtk                        (Button (..), Label (..),
                                                Window (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           Pipes.Prelude                 (repeatM)

data State =
  State
    { count :: Integer
    }

data Event
  = Incr
  | Closed

view' :: State -> AppView Window Event
view' State {..} =
  bin
    Window
    [ #title := "Notebook"
    , on #deleteEvent (const (True, Closed))
    , #widthRequest := 400
    , #heightRequest := 300
    ] $
  notebook
    []
    [ page "First tab" $ widget Label [#label := "Nothing to see here"]
    , page
        (">> " <> pack (show count) <> " <<")
        (widget Label [#label := "Tab labels can be dynamic"])
    , pageWithTab
        (widget Button [#label := "Click me"])
        (widget Label [#label := "Tab labels can contain any widget"])
    ]

update' :: State -> Event -> Transition State Event
update' State {..} Incr = Transition State {count = count + 1} (pure Nothing)
update' State {..} Closed = Exit

main :: IO ()
main =
  void $
  run
    App
      { view = view'
      , update = update'
      , inputs = [incrPeriodically]
      , initialState = State 0
      }
  where
    incrPeriodically = repeatM $ Incr <$ threadDelay (1000 * 1000)
