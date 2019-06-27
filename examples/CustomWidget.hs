{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CustomWidget where

import           Control.Monad                  (void)
import           Data.Typeable                  (Typeable)
import           Data.Word

import qualified GI.GObject                     as GI
import qualified GI.Gtk                         as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           GI.Gtk.Declarative.EventSource (fromCancellation)


-- * Custom widget for ranged 'Double' inputs
-------------------------------------------

data NumberInputProperties = NumberInputProperties
  { value              :: Double
  , range              :: (Double, Double)
  , step               :: Double
  , digits             :: Word32
  , numberInputClasses :: ClassSet
  } deriving (Eq, Show)


newtype NumberInputEvent = NumberInputChanged Double

numberInput
  :: NumberInputProperties
  -> Widget NumberInputEvent
numberInput customParams = Widget
  (CustomWidget
    { customWidget
    , customCreate
    , customPatch
    , customSubscribe
    , customParams
    }
  )
 where
  -- The constructor for the underlying GTK widget.
  customWidget = Gtk.Box
  -- A function that creates a widget (of the same type as
  -- customWidget), used on first render and on 'CustomReplace'. It's
  -- also returning our internal state, a reference to the spin button
  -- widget.
  customCreate props = do
    box <- Gtk.new Gtk.Box [ #orientation Gtk.:= Gtk.OrientationVertical
                           , #spacing Gtk.:= 10
                           ]
    lbl <- Gtk.new Gtk.Label [#label Gtk.:= "I'm a custom wiget."]
    spin <- Gtk.new Gtk.SpinButton []
    adj  <- propsToAdjustment props
    Gtk.spinButtonSetAdjustment spin adj
    Gtk.spinButtonSetDigits spin (digits props)
    #packStart box lbl True True 0
    #packStart box spin False False 0
    return (box, spin)

  -- A function that computes a patch for our custom widget. Here we
  -- compare the params value of type 'NumberInputProperties' to
  -- decide whether to modify the spin button widget or not. Note that
  -- the spin button widget is passed through the internal state.
  customPatch old new spin
    | old == new = CustomKeep
    | otherwise = CustomModify $ \_box -> do
      adj <- propsToAdjustment new
      Gtk.spinButtonSetAdjustment spin adj
      Gtk.spinButtonSetDigits spin (digits new)
      return spin

  -- Finally, we subscribe to widget signals to emit
  -- 'NumberInputChanged' events from the spin button reference
  -- carried by the internal state.
  customSubscribe (spin :: Gtk.SpinButton) _box cb = do
    h <- Gtk.on spin #valueChanged $ cb . NumberInputChanged =<< #getValue spin
    -- This creates a 'Subscription' from an IO action that
    -- disconnects the signal handler.
    return (fromCancellation (GI.signalHandlerDisconnect spin h))

propsToAdjustment :: NumberInputProperties -> IO Gtk.Adjustment
propsToAdjustment NumberInputProperties { value, range, step } =
  Gtk.adjustmentNew
    value
    (fst range)
    (snd range)
    step
    0.1
    0

-- * Example application using the custom widget
------------------------------------------------

data State = State Double

data Event = NumberSet Double | Closed

view' :: State -> AppView Gtk.Window Event
view' (State currentValue) =
  bin
      Gtk.Window
      [ #title := "Hello"
      , on #deleteEvent (const (True, Closed))
      , #widthRequest := 400
      , #heightRequest := 300
      ]
    $ centered (toNumberEvent <$> numberSetter)
 where
  -- Construct our custom widget with some properties for the
  -- underlying SpinButton
  numberSetter = numberInput NumberInputProperties
    { value              = currentValue
    , range              = (0, 10)
    , step               = 0.1
    , digits             = 1
    , numberInputClasses = ["my-number-input"]
    }
  -- Map the custom widget's event to our app 'Event' type
  toNumberEvent (NumberInputChanged d) = NumberSet d

-- Helper that vertically and horizontally centers a widget
centered :: Typeable e => Widget e -> Widget e
centered w = container
  Gtk.Box
  [#orientation := Gtk.OrientationVertical]
  [ BoxChild defaultBoxChildProperties { expand = True, padding = 10 }
      $ container
          Gtk.Box
          [#orientation := Gtk.OrientationHorizontal]
          [BoxChild defaultBoxChildProperties { expand = True, padding = 10 } w]
  ]

update' :: State -> Event -> Transition State Event
update' _ (NumberSet d) = Transition (State d) (return Nothing)
update' _ Closed        = Exit

main :: IO ()
main = void $ run App
  { view         = view'
  , update       = update'
  , inputs       = []
  , initialState = State 1.0
  }
