{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Paned where

import           Control.Monad                  ( void )

import           GI.Gtk                         ( Label(..)
                                                , Window(..)
                                                )
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

type State = ()

data Event = Closed

view' :: AppView Window Event
view' =
  bin
      Window
      [ #title := "Hello"
      , on #deleteEvent (const (True, Closed))
      , #widthRequest := 400
      , #heightRequest := 300
      ]
    $ paned [#wideHandle := True]
            (pane paneProps $ widget Label [#label := "Left"])
            (pane paneProps $ widget Label [#label := "Right"])
  where paneProps = defaultPaneProperties { resize = True }

update' :: State -> Event -> Transition State Event
update' _ Closed = Exit

main :: IO ()
main = void $ run App { view         = const view'
                      , update       = update'
                      , inputs       = []
                      , initialState = ()
                      }
