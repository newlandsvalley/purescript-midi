module Data.Midi
        ( Track(..)
        , Header(..)
        , MidiEvent(..)
        , MidiMessage(..)
        , MidiRecording(..)
        , Ticks
        ) where

import Prelude (class Show, class Eq, class Ord)
import Data.List (List)
import Data.Newtype (class Newtype)
import Data.Generic (gShow, class Generic)

type Ticks =
    Int

-- |  Midi Event
data MidiEvent
    = -- meta messages
      SequenceNumber Int
    | Text String
    | Copyright String
    | TrackName String
    | InstrumentName String
    | Lyrics String
    | Marker String
    | CuePoint String
    | ChannelPrefix Int
    | Tempo Int
    | SMPTEOffset Int Int Int Int Int
    | TimeSignature Int Int Int Int
    | KeySignature Int Int
    | SequencerSpecific String
    | SysEx String
    | Unspecified Int (List Int)
      -- channel messages
    | NoteOn Int Int Int
    | NoteOff Int Int Int
    | NoteAfterTouch Int Int Int
    | ControlChange Int Int Int
    | ProgramChange Int Int
    | ChannelAfterTouch Int Int
    | PitchBend Int Int
    | RunningStatus Int Int

derive instance genericMidiEvent :: Generic MidiEvent
instance showMidiEvent :: Show MidiEvent where
  show = gShow

-- | Midi Message
data MidiMessage = MidiMessage Ticks MidiEvent

derive instance genericMidiMessage :: Generic MidiMessage
instance showMidiMessage :: Show MidiMessage where
  show = gShow


-- | Midi Track
newtype Track = Track
  (List MidiMessage)

derive instance newtypeTrack :: Newtype Track _
derive instance genericTrack :: Generic Track
instance showTrack :: Show Track where
  show = gShow

-- | Midi Header
newtype Header = Header
    { formatType :: Int
    , trackCount :: Int
    , ticksPerBeat :: Int
    }

derive instance newtypeHeader :: Newtype Header _
derive instance eqHeader :: Eq Header
derive instance ordHeader:: Ord Header

derive instance genericHeader :: Generic Header
instance showHeader :: Show Header where
  show = gShow

-- | Midi Recording
newtype MidiRecording = MidiRecording
    { header :: Header
    , tracks :: List Track
    }


derive instance newtypeMidirecording :: Newtype MidiRecording _
derive instance genericMidiRecording :: Generic MidiRecording
instance showMidiRecording :: Show MidiRecording where
  show = gShow
