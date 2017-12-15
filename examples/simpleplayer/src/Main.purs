module Main where


import JS.FileIO (FILEIO, Filespec, loadBinaryFileAsText)

import Audio.SoundFont (AUDIO, Instrument, MidiNote, loadRemoteSoundFonts, playNotes)
import CSS.TextAlign (center, textAlign)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Network.HTTP.Affjax (AJAX)
import Data.Array (null)
import Data.List (length, toUnfoldable)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Midi as Midi
import Data.Midi.Parser (parse, normalise)
import Data.Midi.Instrument (InstrumentName(..))
import MidiPerformance (toPerformance)
import Polyphony (toPolyphonicPerformance)
import Prelude (Unit, bind, const, discard, pure, not, show, ($), (<>), (-))
import Pux (EffModel, noEffects, start)
import Pux.DOM.Events (onChange, onClick)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)
import Pux.Renderer.React (renderToDOM)
import Signal.Channel (CHANNEL)
import Text.Smolder.HTML (div, h1, input, button, p)
import Text.Smolder.HTML.Attributes (type', id, accept)
import Text.Smolder.Markup (Attribute, text, (#!), (!))

data Event
  = NoOp
  | RequestLoadFonts
  | FontLoaded (Array Instrument)
  | RequestFileUpload
  | FileLoaded Filespec
  | PlayTrack Int
  | Play   -- all tracks

-- | the Pux state
type State =
  { instruments :: Array Instrument
  , filespec :: Maybe Filespec
  , recording :: Either String Midi.Recording
  , selectedTrack :: Int
  }

initialState :: State
initialState =
  { instruments : []
  , filespec : Nothing
  , recording : Left "no recording"
  , selectedTrack : 0
  }

foldp :: Event -> State -> EffModel State Event (ajax :: AJAX, au:: AUDIO, fileio :: FILEIO)
foldp NoOp state =  noEffects state
foldp RequestLoadFonts state =
 { state: state
   , effects:
     [ do
         instruments <- loadRemoteSoundFonts [AcousticGrandPiano, Vibraphone, AcousticGuitarNylon, Tuba]
         pure $ Just (FontLoaded instruments)
     ]
  }
foldp (FontLoaded instruments ) state =
  noEffects $ state { instruments = instruments}
foldp RequestFileUpload state =
 { state: state
   , effects:
     [ do
         filespec <- loadBinaryFileAsText "fileinput"
         pure $ Just (FileLoaded filespec)
     ]
  }
foldp (FileLoaded filespec) state =
   noEffects $ processFile filespec state
foldp (PlayTrack trackNum) state =
  case state.recording of
    Right rec ->
      let
        notes = toUnfoldable $ toPerformance rec trackNum
      in
        { state : state { selectedTrack = trackNum }
        , effects:
           [ do
               _ <- liftEff $ playNotes state.instruments notes
               pure $ Just NoOp
           ]
        }
    Left err ->
      noEffects (state { selectedTrack = trackNum })
foldp Play state =
  case state.recording of
    Right rec ->
      let
        notes = toUnfoldable $ toPolyphonicPerformance rec
      in
        { state : state
        , effects:
           [ do
               _ <- liftEff $ playNotes state.instruments notes
               pure $ Just NoOp
           ]
        }
    Left err ->
      noEffects state


processFile :: Filespec -> State -> State
processFile filespec state =
   state { filespec =  Just filespec
         , recording = fullParse filespec.contents
         , selectedTrack = 0
         }

fullParse :: String -> Either String Midi.Recording
fullParse s =
  parse $ normalise s

debugNote  :: MidiNote -> HTML Event
debugNote n =
  text $ (" pitch:" <> show n.id <> " offset:" <> show n.timeOffset)

debugRecordingState :: State -> HTML Event
debugRecordingState state =
  case state.recording of
    Right rec ->
      let
        notes = toPerformance rec state.selectedTrack
        nextTrack = "track " <> (show state.selectedTrack)
      in
        div do
          p $ text (nextTrack <> " has " <> (show $ length notes) <> " notes")
          -- traverse_ debugNote notes
    Left err ->
      do
        p $ text ("error: " <> err)

viewPlayButtons :: State -> HTML Event
viewPlayButtons state =
  case state.recording of
    Right rec ->
      let
        (Midi.Recording recording) = rec
        (Midi.Header header) = recording.header
      in
        case header.trackCount of
          1 ->
            button #! onClick (const $ PlayTrack 0) $ text "play"
          n ->
            do
              trackButtons header.trackCount
              playAllButton
    _ ->
      p $ text ""


trackButtons :: Int -> HTML Event
trackButtons next =
  case next of
    0 ->
      p $ text ""
    n ->
      do
        trackButton (n - 1)
        trackButtons (n - 1)

trackButton :: Int -> HTML Event
trackButton trackNum =
  button #! onClick (const $ PlayTrack trackNum) $ text ("play track " <> show trackNum)

playAllButton :: HTML Event
playAllButton =
  button #! onClick (const Play) $ text "play all"


-- | not ideal.  At the moment we don't catch errors from fonts that don't load
isFontLoaded :: State -> Boolean
isFontLoaded state =
  not $ null state.instruments

view :: State -> HTML Event
view state =
  if (isFontLoaded state) then
    div  do
      h1 ! centreStyle $ text "Play a MIDI file as a single Web-Audio graph"
      div do
        input ! type' "file" ! id "fileinput" ! accept ".midi"
          #! onChange (const RequestFileUpload)
        viewPlayButtons state
        debugRecordingState state
  else
    button #! onClick (const RequestLoadFonts) $ text "load soundfonts"

centreStyle :: Attribute
centreStyle =
  style do
    textAlign center

main :: Eff (ajax :: AJAX, channel :: CHANNEL, exception :: EXCEPTION, fileio :: FILEIO, au :: AUDIO ) Unit
main = do
  app <- start
    { initialState: initialState
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
