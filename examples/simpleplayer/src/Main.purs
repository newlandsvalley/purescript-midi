module Main where

import BinaryFileIO.FileIO
import Data.Midi as Midi
import MidiPerformance (toPerformance)
import Audio.SoundFont (AUDIO, MidiNote, loadRemoteSoundFont, playNotes)
import CSS.TextAlign (center, textAlign)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Midi.Parser (parse, normalise, translateRunningStatus)
import Prelude (Unit, bind, const, pure, show, ($), (<>))
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
  | FontLoaded Boolean
  | RequestFileUpload
  | FileLoaded Filespec
  | Play

-- | the Pux state
type State =
  { fontLoaded :: Boolean
  , filespec :: Maybe Filespec
  , recording :: Maybe Midi.Recording
  }

initialState :: State
initialState =
  { fontLoaded : false
  , filespec : Nothing
  , recording : Nothing
  }

foldp :: Event -> State -> EffModel State Event (au:: AUDIO, fileio :: FILEIO)
foldp NoOp state =  noEffects state
foldp RequestLoadFonts state =
 { state: state
   , effects:
     [ do
         loaded <- loadRemoteSoundFont "acoustic_grand_piano"
         pure $ Just (FontLoaded loaded)
     ]
  }
foldp (FontLoaded loaded) state =
  noEffects $ state { fontLoaded = loaded }
foldp RequestFileUpload state =
 { state: state
   , effects:
     [ do
         filespec <- loadBinaryFile
         pure $ Just (FileLoaded filespec)
     ]
  }
foldp (FileLoaded filespec) state =
   noEffects $ processFile filespec state
foldp Play state =
  case state.recording of
    Just rec ->
      let
        notes = toPerformance rec
      in
        { state : state
        , effects:
           [ do
               _ <- liftEff $ playNotes notes
               pure $ Just NoOp
           ]
        }
    Nothing ->
      noEffects state

processFile :: Filespec -> State -> State
processFile filespec state =
   state { filespec =  Just filespec
         , recording = fullParse filespec.contents
         }

fullParse :: String -> Maybe Midi.Recording
fullParse s =
  case translateRunningStatus $ parse $ normalise $ s of
    Left err ->
      Nothing
    Right midi ->
      Just midi

debugNote  :: MidiNote -> HTML Event
debugNote n =
  text $ (" pitch:" <> show n.id <> " offset:" <> show n.timeOffset)

debugRecordingState :: State -> HTML Event
debugRecordingState state =
  case state.recording of
    Just rec ->
      let
        notes = toPerformance rec
      in
        div do
          -- p $ text ("recording has " <> (show $ length notes) <> " notes")
          traverse_ debugNote notes
    _ ->
      do
        p $ text "got no recording"

viewPlayButton :: State -> HTML Event
viewPlayButton state =
  case state.recording of
    Just rec ->
      button #! onClick (const Play) $ text "play"
    _ ->
      p $ text ""

view :: State -> HTML Event
view state =
  if (state.fontLoaded) then
    div  do
      h1 ! centreStyle $ text "play a MIDI file as a single Web-Audio graph"
      div do
        input ! type' "file" ! id "fileinput" ! accept ".midi"
          #! onChange (const RequestFileUpload)
        viewPlayButton state
        debugRecordingState state
  else
    button #! onClick (const RequestLoadFonts) $ text "load soundfonts"

centreStyle :: Attribute
centreStyle =
  style do
    textAlign center

main :: Eff (channel :: CHANNEL, err :: EXCEPTION, fileio :: FILEIO, au :: AUDIO ) Unit
main = do
  app <- start
    { initialState: initialState
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
