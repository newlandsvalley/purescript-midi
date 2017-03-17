module Main where


import Audio.SoundFont (AUDIO, loadRemoteSoundFont, playNote)
import CSS.TextAlign (textAlign)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array ((:), filter)
import Data.Either (Either(..))
import Data.Generic (gEq, class Generic)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (toNumber)
import Data.Midi (MidiEvent(..))
import Data.Midi.Parser (parseMidiEvent)
import Data.Midi.WebMidi (WEBMIDI, Device, RawMidiEvent, detectInputDevices, listen, webMidiConnect)
import Data.Tuple (Tuple(..))
import Prelude (class Eq, Unit, bind, const, map, show, pure, ($), (<>), (==), (/=), (#), (/), (*))
import Pux (EffModel, renderToDOM, start, noEffects)
import Pux.CSS (center, em, fontSize, marginLeft, marginTop, px, style)
import Pux.Html (Html, Attribute, button, option, select, text, h1, div, p)
import Pux.Html.Attributes (selected)
import Pux.Html.Events (onClick, onChange)
import Signal.Channel (CHANNEL)

data Action
    = NoOp
    | RequestWebMidi              -- Request a connection to WebMidi
    | WebMidiSupported Boolean    -- Is it supported ?
    | DeviceMessage Device        -- a device connection/disconnection message
    | MidiMessage RawMidiEvent    -- a MIDI event message
    | FontLoaded Boolean          -- set when a requested font loads
    | ChangeInstrument String     -- change the MIDI isntrument

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
  , instrument :: String                  -- the name of the instrument to use
  , fontLoaded :: Boolean                 -- is it loaded?
  , maxVolume :: Int                      -- the maximum volume allowed by the volume control
  }

-- | volumes in MIDI range from 0 to 127
volumeCeiling :: Int
volumeCeiling = 127

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
  , instrument : "grand piano"
  , fontLoaded : false
  , maxVolume : (volumeCeiling / 2)  -- start at half volume ceiling
  }

update :: Action -> State -> EffModel State Action (au :: AUDIO, wm :: WEBMIDI)
update NoOp state =  noEffects $ state
update RequestWebMidi state =
 { state: state
   , effects:
     [ do
         -- see if WebMidi is supported
         connected <- liftEff $ webMidiConnect
         pure $ WebMidiSupported connected
     , loadFont "grand piano"
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
update (FontLoaded loaded) state =
   noEffects (state { fontLoaded = loaded })
update (DeviceMessage device) state =
   noEffects $ saveDeviceMessage device state
update (MidiMessage msg) state =
  playMidiEvent msg state
update (ChangeInstrument instrument) state =
  { state: state { instrument = instrument, fontLoaded = false }
  , effects: [loadFont instrument]
  }

saveDeviceMessage :: Device -> State -> State
saveDeviceMessage device state =
   let
     newDevices = case device.connected of
       true ->
         device : (filter (\d -> d.id /= device.id) state.inputDevices)
       false ->
         filter (\d -> d.id /= device.id) state.inputDevices
   in
     state { inputDevices =  newDevices }

loadFont :: forall e. String -> Aff e Action
loadFont instrument =
  do
    -- try to load the soundfont
    let
      fontName =
        fromMaybe "acoustic_grand_piano" $ lookup instrument instrumentsMap
    loaded <- loadRemoteSoundFont fontName
    pure $ FontLoaded loaded

-- | interpret MIDI event messages
-- | at the moment we only respond to:
-- |    NoteOn
-- |    Control Volume
-- |
-- | but obviously this is easily extended to other messages
-- | Also, volume control discriminate neither which
-- | device is being played nor which MIDI channel is in operation
-- | (i.e. you're probably OK if you just attach a single device)
playMidiEvent :: RawMidiEvent -> State -> EffModel State Action (au :: AUDIO, wm :: WEBMIDI)
playMidiEvent msg state =
  if (state.fontLoaded) then
    let
      midiEvent = (parseMidiEvent msg.encodedBinary)
    in
      case midiEvent of
        Right (NoteOn channel pitch velocity) ->
          { state : state
          , effects :
            [ do
                let
                  -- respond to the current volume control setting
                  volumeScale =
                    toNumber state.maxVolume / toNumber volumeCeiling
                  -- and this is what's left of the note
                  gain =
                    toNumber velocity * volumeScale / toNumber volumeCeiling
                  midiNote = { id: pitch, timeOffset: 0.0, duration : 1.0, gain : gain }
                noteLen <- liftEff $ playNote midiNote
                pure $ NoOp
            ]
          }
        _ ->
          let
            newState = recogniseControlMessage midiEvent state
          in
            noEffects newState

  else
   noEffects state

-- | recognise and act on a control message and save to the model state
-- |    At the moment, we just recognise volume changes
recogniseControlMessage :: Either String MidiEvent -> State -> State
recogniseControlMessage event state =
  case event of
    Right (ControlChange channel 7 amount) ->
      state { maxVolume = amount }
    _ ->
      state


-- | mapping of instruments to gleitzman font names
instruments :: Array (Tuple String String)
instruments =
    [ Tuple "grand piano" "acoustic_grand_piano"
    , Tuple "acoustic guitar" "acoustic_guitar_nylon"
    , Tuple "bassoon" "bassoon"
    , Tuple "cello" "cello"
    , Tuple "harp" "orchestral_harp"
    , Tuple "harpsichord" "harpsichord"
    , Tuple "marimba" "marimba"
    , Tuple "oboe" "oboe"
    , Tuple "sitar" "sitar"
    , Tuple "vibraphone" "vibraphone"
    , Tuple "xylophone" "xylophone"
    ]

instrumentsMap :: Map String String
instrumentsMap =
  fromFoldable instruments

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
    Unconnected ->
      p [] [ text $ "Web-Midi is not connected"]
    _ ->
      p [] []

viewFontLoadState :: State -> Html Action
viewFontLoadState state =
  if state.fontLoaded then
    p [] [ text (state.instrument <> " font loaded ")]
  else
    p [] []

instrumentMenu :: State -> Html Action
instrumentMenu state =
  select
    [ selectionStyle
    , onChange (\formEvent -> ChangeInstrument formEvent.target.value)
    ]
       (instrumentOptions state.instrument)

-- | build the drop down list of instruments using the gleitz soundfont instrument name
instrumentOptions :: String -> Array (Html Action)
instrumentOptions target =
  let
    f (Tuple instrument gleitzName) =
        -- option [ selectedInstrument name instrument ]
        option [selected (target == instrument)]
          [ text instrument ]
  in
    map f instruments

view :: State -> Html Action
view state =
  div []
    [
      h1 [ centreStyle ] [ text "Web-Midi keyboard" ]
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
         , (if (state.webMidiConnection == Connected) then
              div []
                [ text "select an instrument"
                , instrumentMenu state
                ]
             else
               p [] []
            )
          , viewFontLoadState state
         ]
    ]

centreStyle :: forall a. Attribute a
centreStyle =
    style $ do
       textAlign center
       -- margin auto

selectionStyle :: forall a. Attribute a
selectionStyle =
    style $ do
      marginLeft(40.0 # px)
      marginTop (20.0 # px)
      fontSize (1.0 # em)

-- main :: forall e. Eff (channel :: CHANNEL, err :: EXCEPTION, fileio :: FILEIO | e) Unit
main :: Eff (channel :: CHANNEL, err :: EXCEPTION, au :: AUDIO, wm :: WEBMIDI ) Unit
main = do
  app <- start
    { initialState: initialState
    , update: update
    , view: view
    , inputs: []
    }

  renderToDOM "#app" app.html
