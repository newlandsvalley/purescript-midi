module Main where


import CSS.TextAlign (textAlign)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array ((:), filter)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Midi.Parser (parseMidiEvent)
import Data.Midi.WebMidi (WEBMIDI, Device, RawMidiEvent, detectInputDevices, listen, webMidiConnect)
import Prelude (class Eq, Unit, bind, const, map, show, pure, ($), (<>), (==), (/=))
import Pux (EffModel, renderToDOM, start, noEffects)
import Pux.CSS (center, style)
import Pux.Html (Html, Attribute, text, button, h1, div, p)
import Pux.Html.Events (onClick)
import Signal.Channel (CHANNEL)
import Data.Generic (gEq, class Generic)

data Action
    = NoOp
    | RequestWebMidi             -- Request a connection to WebMidi
    | WebMidiSupported Boolean   -- Is it supported ?
    | DeviceMessage Device       -- a device connection/disconnection message
    | MidiMessage RawMidiEvent   -- a MIDI event message

data ConnectionState =
    Unconnected
  | Connected
  | Unsupported

derive instance genericConnectionState :: Generic ConnectionState
instance eqConnectionState :: Eq ConnectionState where
  eq = gEq

type State =
  { webMidiConnection :: ConnectionState  -- the state of the WebMidi Connection
  , inputDevices :: Array Device          -- currently attached input devices
  , midiMessage :: Maybe RawMidiEvent     -- the latest MIDI message
  }

{- this is defined within Pux
type EffModel state action eff =
  { state :: state
  , effects :: Array (Aff (channel :: CHANNEL | eff) action)
  }
-}
initialState :: State
initialState = {
    webMidiConnection : Unconnected
  , inputDevices : []
  , midiMessage : Nothing
  }

update :: Action -> State -> EffModel State Action (wm :: WEBMIDI)
update NoOp state =  noEffects $ state
update RequestWebMidi state =
 { state: state
   , effects:
     [ do
        connected <- liftEff $ webMidiConnect
        pure $ WebMidiSupported connected
     ]
  }
update (WebMidiSupported supported) state =
  let
    connectionState =
      if supported then
        Connected
      else
        Unsupported
    newState = state { webMidiConnection = connectionState }
    effects =
      if supported then
        [ do  -- discover the connected input devices
            msg <- detectInputDevices
            pure $ DeviceMessage msg
        , do  -- listen for MIDI messages
             msg <- listen
             pure $ MidiMessage msg
        ]
      else
        []
  in
    {state: newState, effects: effects}
update (DeviceMessage device) state =
   noEffects $ loadDeviceMessage device state
update (MidiMessage msg) state =
    noEffects $ loadMidiMessage msg state

loadMidiMessage :: RawMidiEvent -> State -> State
loadMidiMessage msg state =
   state { midiMessage =  Just msg  }

loadDeviceMessage :: Device -> State -> State
loadDeviceMessage device state =
   let
     newDevices = case device.connected of
       true ->
         device : (filter (\d -> d.id /= device.id) state.inputDevices)
       false ->
         filter (\d -> d.id /= device.id) state.inputDevices
   in
     state { inputDevices =  newDevices }

showDevice :: Device -> Html Action
showDevice device =
  p [] [ text $
    device.name <> " " <> device.id ]

viewDevices :: State -> Array (Html Action)
viewDevices state =
  case state.webMidiConnection of
    Connected ->
      case state.inputDevices of
        [] ->
          [
            p [] [ text "no MIDI device connected" ]
          ]
        _ ->
          map showDevice state.inputDevices
    _ ->
      [
        p [] []
      ]

viewMsg :: Maybe RawMidiEvent -> String
viewMsg Nothing = ""
viewMsg (Just rawEvent) =
  case parseMidiEvent rawEvent.encodedBinary of
    Right event -> show event
    Left error -> show ("parse error: " <> error)

viewConnectionState :: State -> Html Action
viewConnectionState state =
  case state.webMidiConnection of
    Unsupported ->
      p [] [ text $ "Web-Midi is not supported"]
    _ ->
      p [] []

view :: State -> Html Action
view state =
  div []
    [
      h1 [ centreStyle ] [ text "detect Web-Midi input devices" ]
    , div
       []
         [
           (if (state.webMidiConnection == Unconnected) then
             button [onClick (const RequestWebMidi)] [ text "connect to MIDI" ]
           else
             p [] []
           )
         , viewConnectionState state
         , div [] (viewDevices state)
         , p [] [ text $ viewMsg state.midiMessage]
         ]
    ]

centreStyle :: forall a. Attribute a
centreStyle =
    style $ do
       textAlign center
       -- margin auto

-- main :: forall e. Eff (channel :: CHANNEL, err :: EXCEPTION, fileio :: FILEIO | e) Unit
main :: Eff (channel :: CHANNEL, err :: EXCEPTION, wm :: WEBMIDI ) Unit
main = do
  app <- start
    { initialState: initialState
    , update: update
    , view: view
    , inputs: []
    }

  renderToDOM "#app" app.html
