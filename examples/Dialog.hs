{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Example of using the 'Dialog' widget. Note that it's not using
-- the action bar support, nor the response codes mechanism of GTK+
-- dialogs. Those are more object-oriented APIs that do not fit well
-- with gi-gtk-declarative.
module Dialog where

import           Control.Monad                 (void)

import           GI.Gtk                        (Dialog (..), Label (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

data State = Initial

data Event = Confirmed | Cancelled | Closed

view' :: State -> AppView Dialog Event
view' Initial =
  bin Dialog [#title := "Hello", on #deleteEvent (const (True, Closed))] $
    widget Label [#label := "Nothing here yet."]

update' :: State -> Event -> Transition State Event
update' _ Confirmed = Exit
update' _ Cancelled = Exit
update' _ Closed    = Exit

main :: IO ()
main = void $ run App
  { view         = view'
  , update       = update'
  , inputs       = []
  , initialState = Initial
  }

