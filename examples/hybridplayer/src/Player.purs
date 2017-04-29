module Player where

import CSS.TextAlign
import Audio.SoundFont (AUDIO, MidiNote, loadRemoteSoundFont, playNotes)
import BinaryFileIO.FileIO (FILEIO, Filespec, loadBinaryFile)
import CSS (color, fromString)
import CSS.Background (background, backgroundImages)
import CSS.Border (border, borderRadius, solid)
import CSS.Box (boxShadow)
import CSS.Color (rgb, rgba)
import CSS.Display (display, float, floatLeft, inlineBlock, position, relative)
import CSS.Geometry (width, height, padding, margin)
import CSS.Overflow (hidden, overflow)
import CSS.Size (px)
import Control.Monad.Aff (later')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (null, index, length)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Midi.Parser (parse, normalise, translateRunningStatus)
import HybridPerformance (Melody, MidiPhrase, toPerformance)
import Prelude (bind, const, negate, not, show, pure, ($), (<>), (<<<), (||), (+), (*))
import Pux (EffModel, noEffects)
import Pux.DOM.Events (onClick, onChange)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)
import Text.Smolder.HTML (button, div, h1, p, input, progress)
import Text.Smolder.HTML.Attributes (type', id, accept, max, src, value)
import Text.Smolder.Markup (Attribute, text, (#!), (!))

-- import Debug.Trace (trace)

data Event
  = NoOp
  | RequestLoadFonts
  | FontLoaded Boolean
  | RequestFileUpload
  | FileLoaded Filespec
  | StepMidi  Number     -- not called directly but its presence allows a view update
  | PlayMidi Boolean     -- play | pause
  | StopMidi

type PlayerState =
  { playing :: Boolean
  , phraseMax :: Int
  , phraseIndex :: Int
  , lastPhraseLength :: Number
  }

type State =
  { filespec :: Maybe Filespec
  , melody :: Melody
  , fontLoaded :: Boolean                 -- is the soundfount loaded?
  , playerState :: PlayerState
  }

initialPlayerState :: Int -> PlayerState
initialPlayerState max =
  { playing : false
  , phraseMax : max
  , phraseIndex : 0
  , lastPhraseLength : 0.0
  }

initialState :: State
initialState =
  { filespec : Nothing
  , melody : []
  , fontLoaded : false
  , playerState : initialPlayerState 0
  }

-- foldp :: Event -> State -> EffModel State Action
foldp :: ∀ fx. Event -> State -> EffModel State Event (fileio :: FILEIO, au :: AUDIO | fx)
foldp NoOp state =  noEffects $ state
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
foldp (StepMidi delay) state =  step state delay
foldp (PlayMidi playing) state =
  let
    newPlayerState = state.playerState { playing = playing }
    newState = state { playerState = newPlayerState }
  in
    step newState 0.0
foldp (StopMidi) state =
  let
    playerState =
      state.playerState { phraseIndex = 0
                        , playing = false }
  in
    noEffects $ state { playerState = playerState }

processFile :: Filespec -> State -> State
processFile filespec state =
  let
    recording = (translateRunningStatus <<< parse <<< normalise) filespec.contents
    melody =
      case recording of
        Right rec -> toPerformance rec
        _ -> []
  in
    state { filespec =  Just filespec
          , melody = melody
          , playerState = initialPlayerState (length melody)
          }

-- | step through the MIDI events, one by one
step :: forall e. State -> Number -> EffModel State Event (au :: AUDIO | e)
step state delay =
  case locateNextPhrase state of
    Just (midiPhrase) ->
      let
        msDelay = round $ delay * 1000.0
        -- set the new state
        newPlayerState =
          state.playerState { phraseIndex = state.playerState.phraseIndex + 1
                            , lastPhraseLength = delay
                            }
      in
        { state: state { playerState = newPlayerState }
        , effects:
          [ do
              nextDelay <-
                  later' msDelay $ liftEff (playEvent midiPhrase)
              pure $ Just (StepMidi nextDelay)
          ]
        }
    _ ->
      noEffects state

-- | play a MIDI Phrase (a bunch of MIDI notes)
-- | only NoteOn events produce sound
playEvent :: forall eff. MidiPhrase -> Eff (au :: AUDIO | eff) Number
playEvent midiPhrase =
  playNotes midiPhrase

-- | locate the next MIDI phrase from the performance
locateNextPhrase :: State -> Maybe MidiPhrase
locateNextPhrase state =
  if (not state.playerState.playing) || (null state.melody) then
    Nothing
  else
    index state.melody (state.playerState.phraseIndex)

-- | just display the next MIDI phrase index
{-
viewPhraseIndex :: State -> HTML Event
viewPhraseIndex state =
  p $ text (show state.playerState.phraseIndex)
-}

debugNote  :: MidiNote -> HTML Event
debugNote n =
  text $ ("{ pitch:" <> show n.id <> " offset:" <> show n.timeOffset <> " length:" <> show n.duration <> " }")

debugPhrase :: MidiPhrase -> HTML Event
debugPhrase phrase =
  div do
    traverse_ debugNote phrase

debugMelody :: State -> HTML Event
debugMelody state =
  traverse_ debugPhrase state.melody

{-
debugMelody :: State -> HTML Event
debugMelody state =
  let
    mPhrase = index state.melody state.playerState.phraseIndex
  in
    case mPhrase of
      Just phrase ->
        do
          debugPhrase phrase
          -- p $ text ("total phrase delay: " <> show state.playerState.lastPhraseLength)
      _ -> p $ text ""
-}

view :: State -> HTML Event
view state =
  if (state.fontLoaded) then
     div  do
       h1 ! centreStyle $ text "Hybrid MIDI file player"
       div do
         input ! type' "file" ! id "fileinput" ! accept ".midi"
           #! onChange (const RequestFileUpload)
         player state
         div do
           debugMelody state
  else
    button #! onClick (const RequestLoadFonts) $ text "load soundfonts"

player :: State -> HTML Event
player state =
  let
    sliderPos = show state.playerState.phraseIndex

    startImg = "assets/images/play.png"
    stopImg =  "assets/images/stop.png"
    pauseImg = "assets/images/pause.png"
    playAction =
      if state.playerState.playing then
         PlayMidi false
      else
         PlayMidi true
    playButtonImg =
      if state.playerState.playing then
        pauseImg
      else
        startImg
    capsuleMax =
      show state.playerState.phraseMax
  in
    case state.melody of
      [] ->
        p $ text ""
      _ ->
        div ! playerBlockStyle $ do
          div ! playerBaseStyle ! playerStyle $ do
            progress ! capsuleStyle ! max capsuleMax ! value sliderPos $ do
              text ""
            div ! buttonStyle $ do
              input ! type' "image" ! src playButtonImg
                 #! onClick (const playAction)
              input ! type' "image" ! src stopImg
                 #! onClick (const StopMidi)

centreStyle :: Attribute
centreStyle =
  style do
    textAlign center

-- | the capsule is the bit in the centre of the player widget that shows progress
-- | through the recording
capsuleStyle :: Attribute
capsuleStyle =
  style do
    border solid (px 1.0) (rgb 0 0 0)
    margin (px 8.0) (px 0.0) (px 8.0) (px 0.0) -- ??
    borderRadius (px 5.0) (px 5.0) (px 5.0) (px 5.0)
    -- backgroundColor (rgb 0 0 0)
    background (rgb 0 0 0)
    backgroundImages
      [ fromString "-webkit-gradient(linear, left top, left bottom, color-stop(1, rgba(0,0,0,0.5)), color-stop(0, #333))"
      , fromString "-webkit-linear-gradient(top, rgba(0, 0, 0, 0.5) 1, #333 0)"
      , fromString "-moz-linear-gradient(top, rgba(0, 0, 0, 0.5) 1, #333 0)"
      , fromString "-ms-gradient(top, rgba(0, 0, 0, 0.5) 1, #333 0)"
      , fromString "-o-gradient(top, rgba(0, 0, 0, 0.5) 1, #333 0)"
      , fromString "linear-gradient(top, rgba(0, 0, 0, 0.5) 1, #333 0)"
      ]
    boxShadow (px 0.0) (px 0.0) (px 0.0) (rgb 5 5 5)
    overflow hidden
    display inlineBlock
    width (px 220.0)
    height (px 20.0)


-- | the basic style of the outline of the player which surrounds
-- | both the buttons and the capsule
playerBlockStyle :: Attribute
playerBlockStyle =
  style do
    background (rgba 0 0 0 0.7)
    border solid (px 1.0) (rgb 0 0 0)
    borderRadius (px 10.0) (px 10.0) (px 10.0) (px 10.0)
    width (px 330.0)
    position relative  -- "relative; z-index: 2"

-- the style of the player
playerStyle :: Attribute
playerStyle =
  style do
    height (px 36.0)
    boxShadow (px (-1.0)) (px (-1.0)) (px (-1.0))  (rgb 0 0 0)
    borderRadius (px 10.0) (px 10.0) (px 10.0) (px 10.0)

-- more player style attributes
playerBaseStyle :: Attribute
playerBaseStyle =
  style do
    border solid (px 1.0) (rgb 0 0 0)
    backgroundImages
      [ fromString "-webkit-gradient(linear,left top,left bottom,from(rgba(66,66,66,1)),to(rgba(22,22,22,1)))"
      , fromString "-webkit-linear-gradient(top, rgba(66, 66, 66, 1) 0%, rgba(22, 22, 22, 1) 100%)"
      , fromString "-moz-linear-gradient(top, rgba(66, 66, 66, 1) 0%, rgba(22, 22, 22, 1) 100%)"
      , fromString "-ms-gradient(top, rgba(66, 66, 66, 1) 0%, rgba(22, 22, 22, 1) 100%)"
      , fromString "-o-gradient(top, rgba(66, 66, 66, 1) 0%, rgba(22, 22, 22, 1) 100%)"
      , fromString "linear-gradient(top, rgba(66, 66, 66, 1) 0%, rgba(22, 22, 22, 1) 100%)"
      ]
    boxShadow (px 0.0) (px 0.0) (px 10.0) (rgb 15 15 15) -- #fff
    borderRadius (px 10.0) (px 10.0) (px 10.0) (px 10.0)
    padding (px 15.0) (px 20.0) (px 15.0) (px 20.0)
    color (rgba 255 255 255 0.8)
    -- "text-shadow", "1px 1px 2px #000"  ???

-- player button styling
buttonStyle :: Attribute
buttonStyle =
  style do
    width (px 80.0)
    margin (px 2.0) (px 3.0) (px 0.0) (px 3.0)
    float floatLeft
    -- opacity 0.7
