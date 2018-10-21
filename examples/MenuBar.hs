{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module MenuBar where

import           Data.Text                      ( Text )
import           GI.Gtk                         ( Box(..)
                                                , Label(..)
                                                , MenuBar(..)
                                                , MenuItem(..)
                                                , Orientation(..)
                                                , Window(..)
                                                )
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

data State = Message Text

data Event = Open | Save | Help | Closed

view' :: State -> AppView Event
view' (Message msg) =
  bin Window [#title := "MenuBar", on #deleteEvent (const (True, Closed)), #widthRequest := 400, #heightRequest := 300]
    $ container Box [#orientation := OrientationVertical]
    $ do
        boxChild False False 0 $ container MenuBar [] $ do
          subMenu "File" $ do
            menuItem MenuItem [on #activate Open]
              $ widget Label [#label := "Open"]
            menuItem MenuItem [on #activate Save]
              $ widget Label [#label := "Save"]
          menuItem MenuItem [on #activate Help]
            $ widget Label [#label := "Help"]
        boxChild True False 0 $ widget Label [#label := msg]

update' :: State -> Event -> Transition State Event
update' _ = \case
  Open   -> Transition (Message "Opening file...") (return Nothing)
  Save   -> Transition (Message "Saving file...") (return Nothing)
  Help   -> Transition (Message "There is no help.") (return Nothing)
  Closed -> Exit

main :: IO ()
main = run App
  { view         = view'
  , update       = update'
  , inputs       = []
  , initialState = Message "Click a button in the menu."
  }
