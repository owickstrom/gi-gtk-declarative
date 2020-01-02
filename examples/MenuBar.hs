{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module MenuBar where

import           Control.Monad                  ( void )
import           Data.Text                      ( Text )
import           GI.Gtk                         ( ApplicationWindow(..)
                                                , Box(..)
                                                , Label(..)
                                                , MenuBar(..)
                                                , MenuItem(..)
                                                , Orientation(..)
                                                )
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

newtype State = Message Text

data Event = Open | Save | Help | Closed

view' :: State -> AppView ApplicationWindow Event
view' (Message msg) =
  bin
      ApplicationWindow
      [ #title := "MenuBar"
      , on #deleteEvent (const (True, Closed))
      , #widthRequest := 400
      , #heightRequest := 300
      ]
    $ container
        Box
        [#orientation := OrientationVertical]
        [ container
          MenuBar
          []
          [ subMenu
            "File"
            [ menuItem MenuItem [on #activate Open]
              $ widget Label [#label := "Open"]
            , menuItem MenuItem [on #activate Save]
              $ widget Label [#label := "Save"]
            ]
          , menuItem MenuItem [on #activate Help]
            $ widget Label [#label := "Help"]
          ]
        , BoxChild defaultBoxChildProperties { expand = True }
          $ widget Label [#label := msg]
        ]

update' :: State -> Event -> Transition State Event
update' _ = \case
  Open   -> Transition (Message "Opening file...") (return Nothing)
  Save   -> Transition (Message "Saving file...") (return Nothing)
  Help   -> Transition (Message "There is no help.") (return Nothing)
  Closed -> Exit

main :: IO ()
main = void $ run App { view         = view'
                      , update       = update'
                      , inputs       = []
                      , initialState = Message "Click a button in the menu."
                      }
