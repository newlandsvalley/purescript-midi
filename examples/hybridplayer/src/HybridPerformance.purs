module HybridPerformance (Melody, MidiPhrase, toPerformance) where

import Control.Monad.State as ControlState
import Audio.SoundFont (MidiNote)
import Data.Midi as Midi
import Data.Tuple (Tuple(..), fst, snd)
import Data.Array ((:), reverse)
import Data.List (List(..), head)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Int (toNumber)
import Prelude (bind, map, pure, ($), (*), (+), (-), (/), (>))

type MidiPhrase = Array MidiNote
type Melody = Array MidiPhrase

-- | a PartialNote comes from MIDI NoteOn
-- | and is incomplete because it has no end time offset/duration
type PartialNote =
  { pitch :: Int
  , gain :: Number
  , timeOffset :: Number
  }

-- | lookup allowing us to match NoteOn and NoteOff events
type NoteMap = Map.Map Int PartialNote

-- | the state to thread through the computation
-- | which translates MIDI to a Web-Audio graph
type TState =
    { ticksPerBeat :: Int         -- taken from the MIDI header
    , tempo :: Int                -- this will almost certainly be reset at the start of the MIDI file
    , noteOffset :: Number        -- the overall time offset within the tune for the next note to be processed
    , phraseOffset :: Number      -- the offset of the current phrase
    , currentPhrase :: MidiPhrase -- the current phrase being built from completed notes
    , currentNoteMap :: NoteMap   -- the set of notes currently being built
    , melody :: Melody            -- the ever-increasing set of generated phrases
    }

type TransformationState =
  Tuple TState Melody

-- | the max size of a phrase (in seconds) between which view updates can occur
-- | vary this to get a good balance between playing the melody in time
-- | and being able to update the UI
phraseSize :: Number
phraseSize = 0.6

initialTState :: Int -> TState
initialTState ticksPerBeat =
  { ticksPerBeat : ticksPerBeat
  , tempo : 1000000
  , noteOffset : 0.0
  , phraseOffset : 0.0
  , currentPhrase : []
  , currentNoteMap : Map.empty
  , melody : []
  }

initialTransformationState :: Int -> TransformationState
initialTransformationState ticksPerBeat =
  Tuple (initialTState ticksPerBeat) []

-- | transform a Midi.Recording to a MIDI performance
-- | which is simply an array of MIDI notes, each timed to
-- | be played at the appropriate time offset
toPerformance :: Midi.Recording -> Melody
toPerformance recording =
  let
    mtrack0 :: Maybe (List Midi.Message)
    mtrack0 = map unwrap $ head (unwrap recording).tracks
    header = unwrap (unwrap recording).header
    track0 :: List Midi.Message
    track0 = fromMaybe Nil mtrack0
  in
    do
      ControlState.evalState (transformTrack track0) (initialTransformationState header.ticksPerBeat)

transformTrack :: List Midi.Message -> ControlState.State TransformationState Melody
transformTrack Nil =
  do
    retrieveMelody
transformTrack (Cons m ms) =
  do
    transformMessage m
    transformTrack ms

transformMessage :: Midi.Message -> ControlState.State TransformationState Melody
transformMessage m =
  case m of
    Midi.Message ticks (Midi.Tempo tempo) ->
      accumulateTempo ticks tempo
    Midi.Message ticks (Midi.NoteOn channel pitch velocity) ->
      if (pitch > 0) then
        accumulateNote addNoteOn ticks channel pitch velocity
      else
        accumulateNote finaliseNote ticks channel pitch velocity
    Midi.Message ticks (Midi.NoteOff channel pitch velocity) ->
      accumulateNote finaliseNote ticks channel pitch velocity
    Midi.Message ticks _ ->
      accumulateTicks ticks

-- | Process a NoteOn or NoteOff MIDI message
accumulateNote :: (Int -> Int -> Int -> Number -> TState -> TState)
                    -> Int -> Int -> Int -> Int -> ControlState.State TransformationState Melody
accumulateNote processNote ticks channel pitch velocity =
  do
    tpl <- ControlState.get
    let
      recording = snd tpl
      tstate = fst tpl
      offset = tstate.noteOffset + ticksToTime ticks tstate
      tstate' = processNote channel pitch velocity offset tstate
      tpl' = Tuple tstate' recording
    _ <- ControlState.put tpl'
    pure recording

accumulateTicks :: Int -> ControlState.State TransformationState Melody
accumulateTicks ticks =
  do
    tpl <- ControlState.get
    let
      recording = snd tpl
      tstate = fst tpl
      offset = tstate.noteOffset + ticksToTime ticks tstate
      tstate' =
        tstate { noteOffset = offset }
      tpl' = Tuple tstate' recording
    _ <- ControlState.put tpl'
    pure recording

accumulateTempo :: Int -> Int -> ControlState.State TransformationState Melody
accumulateTempo ticks tempo =
  do
    tpl <- ControlState.get
    let
      recording = snd tpl
      tstate = fst tpl
      offset = tstate.noteOffset + ticksToTime ticks tstate
      tstate' = tstate { noteOffset = offset
                       , tempo = tempo
                       }
      tpl' = Tuple tstate' recording
    _ <- ControlState.put tpl'
    pure recording

-- | add a note to state. The note is half-built
-- | we still need the duration from the matching NoteOff
addNoteOn :: Int -> Int -> Int -> Number -> TState -> TState
addNoteOn channel pitch velocity offset tstate =
  let
    partialNote = buildPartialNote pitch velocity offset
    key = noteKey channel pitch
    currentNoteMap = Map.insert key partialNote tstate.currentNoteMap
    noteOffset = partialNote.timeOffset
  in
    tstate { noteOffset = noteOffset
           , currentNoteMap = currentNoteMap
           }

-- | finalise the note once the NoteOff message arrives
-- | the note should exist in the map which would
-- | be established by a previous NoteOn but neewds a
-- | finalising duration
finaliseNote :: Int -> Int -> Int -> Number -> TState -> TState
finaliseNote channel pitch velocity endOffset tstate =
  let
    key = noteKey channel pitch
    mpnote = Map.lookup key tstate.currentNoteMap
  in
    case mpnote of
      Just pnote ->
        let
          finalisedNote =
            { id : pnote.pitch
            , timeOffset : pnote.timeOffset - tstate.phraseOffset
            , duration : endOffset - pnote.timeOffset
            , gain : pnote.gain
            }
          currentNoteMap = Map.delete key tstate.currentNoteMap
          currentPhrase = finalisedNote : tstate.currentPhrase
        in
          -- split into phrases
          if (endOffset - tstate.phraseOffset) > phraseSize then
            tstate { currentNoteMap = currentNoteMap
                   , noteOffset = endOffset
                   , phraseOffset = endOffset
                   , currentPhrase = []
                   , melody = (reverse currentPhrase) : tstate.melody
                   }
          else
            tstate { currentNoteMap = currentNoteMap
                   , noteOffset = endOffset
                   , currentPhrase = currentPhrase
                   }
      _ ->
        tstate { noteOffset = endOffset }

-- | we'll use a mashup of the channel and the pitch as a key
-- | but for all the music I'm interested in, there's only one channel
noteKey :: Int -> Int -> Int
noteKey channel pitch =
   1000 * channel + pitch

-- | convert ticks (at the governing tempo) to time (seconds)
ticksToTime  :: Int -> TState -> Number
ticksToTime ticks tstate =
  (toNumber ticks * toNumber tstate.tempo) / (toNumber tstate.ticksPerBeat * 1000000.0)

retrieveMelody :: ControlState.State TransformationState Melody
retrieveMelody =
  do
    tpl <- ControlState.get
    let
      tstate = fst tpl
      melody = reverse $ (reverse tstate.currentPhrase) : tstate.melody
    pure melody

buildPartialNote :: Int -> Int -> Number -> PartialNote
buildPartialNote pitch velocity timeOffset =
  let
    maxVolume = 127
    gain =
      toNumber velocity / toNumber maxVolume
  in
    { pitch : pitch, gain : gain, timeOffset : timeOffset }
