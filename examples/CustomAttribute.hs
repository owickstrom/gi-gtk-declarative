{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module CustomAttribute where

import           Control.Monad                  (void)
import           Data.Vector                    (Vector)
import           Data.Word

import qualified GI.GObject                     as GI
import qualified GI.Gtk                         as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           GI.Gtk.Declarative.EventSource (fromCancellation)


-- * Custom widget for ranged 'Double' inputs
-------------------------------------------

numberInput
  :: Vector (Attribute Gtk.Box event)
  -> NumberInputProperties
  -> Maybe (Double -> event)
  -> Widget event
numberInput attrs props onInputChanged =
  let customAttr = customAttribute (NumberInput props onInputChanged)
      vertical = #orientation := Gtk.OrientationVertical
   in widget Gtk.Box ([vertical, customAttr] <> attrs)

data NumberInput event = NumberInput
  { props          :: NumberInputProperties
  , onInputChanged :: Maybe (Double -> event)
  }
  deriving (Functor)

data NumberInputProperties = NumberInputProperties
  { value  :: Double
  , range  :: (Double, Double)
  , step   :: Double
  , digits :: Word32
  } deriving (Eq, Show)

instance CustomAttribute Gtk.Box NumberInput where

  data AttrState NumberInput = NumberInputState Gtk.SpinButton

  attrCreate box (NumberInput props _) = do
    lbl  <- Gtk.new Gtk.Label [#label Gtk.:= "I'm a custom widget."]
    spin <- Gtk.new Gtk.SpinButton []
    adj  <- propsToAdjustment props
    Gtk.spinButtonSetAdjustment spin adj
    Gtk.spinButtonSetDigits spin (digits props)
    #packStart box lbl True True 0
    #packStart box spin False False 0
    return (NumberInputState spin)

  attrPatch box state@(NumberInputState spin) (NumberInput oldProps _) (NumberInput newProps _)
    | oldProps == newProps = pure state
    | otherwise = do
        adj <- propsToAdjustment newProps
        Gtk.spinButtonSetAdjustment spin adj
        Gtk.spinButtonSetDigits spin (digits newProps)
        return state

  attrSubscribe box (NumberInputState spin) (NumberInput props onInputChanged) cb = do
    case onInputChanged of
      Nothing -> mempty
      Just handler -> do
        h <- Gtk.on spin #valueChanged $ cb . handler =<< #getValue spin
        -- This creates a 'Subscription' from an IO action that
        -- disconnects the signal handler.
        return (fromCancellation (GI.signalHandlerDisconnect spin h))

propsToAdjustment :: NumberInputProperties -> IO Gtk.Adjustment
propsToAdjustment NumberInputProperties { value, range = (begin, end), step } =
  Gtk.adjustmentNew value begin end step 0.1 0

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
    $ centered numberSetter
 where
  -- Construct our custom widget with some properties for the
  -- underlying SpinButton
  numberSetter = numberInput
    -- Pass attributes (classes or GTK properties) to the widget
    [ classes ["my-number-input"]
    , #spacing := round (currentValue * 10) -- weird, but fun
    ]
    -- And our custom properties
    NumberInputProperties { value  = currentValue
                          , range  = (0, 10)
                          , step   = 0.1
                          , digits = 1
                          }
    -- And our event handler
    (Just NumberSet)

-- Helper that vertically and horizontally centers a widget
centered :: Widget e -> Widget e
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
main = void $ run App { view         = view'
                      , update       = update'
                      , inputs       = []
                      , initialState = State 1.0
                      }
