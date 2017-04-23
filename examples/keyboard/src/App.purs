module App where

import Audio.SoundFont (AUDIO, MidiNote, loadRemoteSoundFont, playNote)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array ((:), filter, null)
import Data.Either (Either(..))
import Data.Generic (gEq, class Generic)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (toNumber)
import Data.Midi as Midi
import Data.Midi.Parser (parseMidiEvent)
import Data.Midi.WebMidi (WEBMIDI, Device, RawMidiEvent, detectInputDevices, listen, webMidiConnect)
import Data.Tuple (Tuple(..))
import Data.Foldable (traverse_)
import Prelude (class Eq, bind, const, not, pure, ($), (<>), (==), (/=), (/), (*), (&&))
import Pux (EffModel, noEffects)
import Pux.DOM.Events (onClick, onChange, targetValue)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)
import Text.Smolder.HTML (button, div, h1, p, option, select)
import Text.Smolder.HTML.Attributes (selected)
import Text.Smolder.Markup (Attribute, text, (#!), (!))
import CSS.Geometry (margin)
import CSS.TextAlign (center, textAlign)
import CSS.Size (px, em)
import CSS.Font (fontSize)

data Event
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

initialState :: State
initialState = {
    webMidiConnection : Unconnected
  , inputDevices : []
  , instrument : "grand piano"
  , fontLoaded : false
  , maxVolume : (volumeCeiling / 2)  -- start at half volume ceiling
  }

foldp :: Event -> State -> EffModel State Event (au :: AUDIO, wm :: WEBMIDI)
foldp NoOp state =  noEffects $ state
foldp RequestWebMidi state =
 { state: state
   , effects:
     [ do
         -- see if WebMidi is supported
         connected <- liftEff $ webMidiConnect
         pure $ Just (WebMidiSupported connected)
     , loadFont "grand piano"
     ]
  }
foldp (WebMidiSupported supported) state =
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
            pure $ Just (DeviceMessage msg)
        , do  -- listen for MIDI messages
            msg <- listen
            pure $ Just (MidiMessage msg)
        ]
      else
        []
  in
    {state: newState, effects: effects}
foldp (FontLoaded loaded) state =
   noEffects (state { fontLoaded = loaded })
foldp (DeviceMessage device) state =
   noEffects $ saveDeviceMessage device state
foldp (MidiMessage msg) state =
  playMidiEvent msg state
foldp (ChangeInstrument instrument) state =
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

loadFont :: forall e. String -> Aff e (Maybe Event)
loadFont instrument =
  do
    -- try to load the soundfont
    let
      fontName =
        fromMaybe "acoustic_grand_piano" $ lookup instrument instrumentsMap
    loaded <- loadRemoteSoundFont fontName
    pure $ Just (FontLoaded loaded)

-- | interpret MIDI event messages
-- | at the moment we only respond to:
-- |    NoteOn
-- |    Control Volume
-- |
-- | but obviously this is easily extended to other messages
-- | Also, volume control discriminate neither which
-- | device is being played nor which MIDI channel is in operation
-- | (i.e. you're probably OK if you just attach a single device)
playMidiEvent :: RawMidiEvent -> State -> EffModel State Event (au :: AUDIO, wm :: WEBMIDI)
playMidiEvent msg state =
  if (state.fontLoaded) then
    let
      midiEvent = (parseMidiEvent msg.encodedBinary)
    in
      case midiEvent of
        Right (Midi.NoteOn channel pitch velocity) ->
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
                  midiNote :: MidiNote
                  midiNote = { id: pitch, timeOffset: 0.0, duration : 1.0, gain : gain }
                _ <- liftEff $ playNote midiNote
                pure $ Just NoOp
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
recogniseControlMessage :: Either String Midi.Event -> State -> State
recogniseControlMessage event state =
  case event of
    Right (Midi.ControlChange channel 7 amount) ->
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

showDevice :: Device -> HTML Event
showDevice device =
  do
    p $ text $ device.name <> " " <> device.id

-- | view the connected MIDI input devices
viewDevices :: State -> HTML Event
viewDevices state =
  case state.webMidiConnection of
    Connected ->
      case state.inputDevices of
        [] ->
          do
            p $ text $ "no MIDI device connected"
        _ ->
          do
            traverse_ showDevice state.inputDevices
    _ ->
      do
        p $ text ""

-- | display the Web-Midi connection state
viewConnectionState :: State -> HTML Event
viewConnectionState state =
  case state.webMidiConnection of
    Unsupported ->
      do
        p $ text $ "Web-Midi is not supported"
    Unconnected ->
      do
        p $ text $ "Web-Midi is not connected"
    _ ->
      do
        p $ text ""

-- | display the status of the soundfont load
viewFontLoadState :: State -> HTML Event
viewFontLoadState state =
  if state.fontLoaded then
    do
      p $ text $ (state.instrument <> " font loaded ")
  else
    do
      p $ text ""

-- | display the instrument menu (if we have a connected device)
instrumentMenu :: State -> HTML Event
instrumentMenu state =
  if (state.webMidiConnection == Connected) && (not null state.inputDevices) then
    div do
      text "select an instrument"
      select ! selectionStyle #! onChange (\e -> ChangeInstrument (targetValue e) )
        $ (instrumentOptions state.instrument)
    else
      do
        p $ text ""

-- | build the drop down list of instruments using the gleitz soundfont instrument name
instrumentOptions :: String -> HTML Event
instrumentOptions target =
  let
    f (Tuple instrument gleitzName) =
        -- option [ selectedInstrument name instrument ]
        if (target == instrument) then
          option ! selected "selected" $ text instrument
        else
          option $ text instrument
  in
    traverse_ f instruments

-- | display the button to connect to Web-Midi if not connected
webMidiConnectButton :: State -> HTML Event
webMidiConnectButton state =
  if (state.webMidiConnection == Unconnected) then
    do
      button #! onClick (const RequestWebMidi) $ text "connect to MIDI"
  else
    do
      p $ text ""

view :: State -> HTML Event
view state =
  div  do
     h1 ! centreStyle $ text "Web-Midi keyboard"
     webMidiConnectButton state
     viewConnectionState state
     viewDevices state
     instrumentMenu state
     viewFontLoadState state

centreStyle :: Attribute
centreStyle =
  style do
    textAlign center

selectionStyle :: Attribute
selectionStyle  =
  style do
    margin (px 20.0) (px 0.0) (px 0.0) (px 40.0)
    fontSize (em 1.0)
