{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Grid where

import           Control.Monad                  ( void )
import           GI.Gtk                         ( Button(..)
                                                , Grid(..)
                                                , Window(..)
                                                )
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           GI.Gtk.Declarative.Container.Grid

data State =
  State

data Event =
  Closed

view' :: State -> AppView Window Event
view' State =
  bin
      Window
      [ #title := "Grid"
      , on #deleteEvent (const (True, Closed))
      , #widthRequest := 400
      , #heightRequest := 300
      ]
    $ container
        Grid
        [#rowSpacing := 4, #columnSpacing := 4, #margin := 4]
        [ GridChild
          { properties = defaultGridChildProperties { width = 3, height = 3 }
          , child      = widget Button [#label := "A", #vexpand := True]
          }
        , GridChild
          { properties = defaultGridChildProperties { width      = 3
                                                    , height     = 1
                                                    , leftAttach = 3
                                                    }
          , child = widget Button
                           [#label := "B", #hexpand := True, #vexpand := True]
          }
        , GridChild
          { properties = defaultGridChildProperties { width      = 2
                                                    , height     = 2
                                                    , leftAttach = 3
                                                    , topAttach  = 1
                                                    }
          , child = widget Button
                           [#label := "C", #hexpand := True, #vexpand := True]
          }
        , GridChild
          { properties = defaultGridChildProperties { width      = 1
                                                    , height     = 1
                                                    , leftAttach = 5
                                                    , topAttach  = 2
                                                    }
          , child      = widget Button [#label := "D"]
          }
        ]

update' :: State -> Event -> Transition State Event
update' State Closed = Exit

main :: IO ()
main = void $ run App { view         = view'
                      , update       = update'
                      , inputs       = []
                      , initialState = State
                      }
