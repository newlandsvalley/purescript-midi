-- | Data structures used by the MIDI parser and by Web MIDI.
module Data.Midi
        ( Track(..)
        , Header(..)
        , Event(..)
        , TimedEvent(..)
        , Message(..)
        , Recording(..)
        , Ticks
        , Byte
        , Channel
        , Note
        , Velocity
        , SysExFlavour(..)
        ) where

import Prelude (class Show, class Eq, class Ord)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList) as Nel
import Data.Maybe (Maybe)
import Data.Generic.Rep
import Data.Eq.Generic (genericEq)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)

-- | A Tick represents a MIDI time increment.  See the MIDI Specification,
-- | page 135 - delta-time.
type Ticks = Int

-- | A MIDI byte (for use in a Byte array) and represented as an Int.
type Byte = Int

-- | A MIDI Channel in the range ( 0 <= channel <= 15).
-- | See the MIDI specification - page 7.
type Channel = Int

-- | A MIDI note number representing a pitch in the range (0 <= note <= 127).
-- | See the MIDI specification page 42 - Note Number.
-- | 0 in a NoteOn message is equivalent to NoteOff.
type Note = Int

-- | An indication of the pressure applied to a key on a MIDI isntrument and
-- | hence of note volume. See the MIDI specification page 42 - Volume.
type Velocity = Int

-- | System exclusive messages exist in two different flavours as introduced by
-- | a lead-in byte of 0xF0 or 0xF7.  See the MIDI specification page 135.
data SysExFlavour
    = F0 -- normal
    | F7 -- escaped

derive instance genericSysExFlavour :: Generic SysExFlavour _
instance showSysExFlavour :: Show SysExFlavour where
  show = genericShow
instance eqSysExFlavour :: Eq SysExFlavour where
  eq = genericEq
instance ordSysExFlavour :: Ord SysExFlavour where
  compare = genericCompare

-- | A Midi Event.
-- |
-- | Note that RunningStatus messages are not included within Event
-- | because the parser translates them to the underlying channel messages
data Event
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
    | SequencerSpecific (List Byte)
      -- system exclusive message
    | SysEx SysExFlavour (Nel.NonEmptyList Byte)
    | Unspecified Int (List Byte)
      -- channel messages
    | NoteOn Channel Note Velocity
    | NoteOff Channel Note Velocity
    | NoteAfterTouch Channel Note Velocity
    | ControlChange Channel Int Int
    | ProgramChange Channel Int
    | ChannelAfterTouch Channel Velocity
    | PitchBend Channel Int

derive instance genericEvent :: Generic Event _
instance showEvent :: Show Event where
  show = genericShow
instance eqEvent :: Eq Event where
  eq = genericEq
instance ordEvent :: Ord Event where
  compare = genericCompare

-- | a timestamped parsed MIDI Event message for use with Web MIDI
newtype TimedEvent = TimedEvent {
    timeStamp :: Number
  , event     :: Maybe Event
  }

derive instance genericTimedEvent:: Generic TimedEvent _

instance showTimedEvent :: Show TimedEvent where
  show = genericShow

-- | A MIDI Message.
data Message = Message Ticks Event

derive instance genericMessage :: Generic Message _
instance showMessage :: Show Message where
  show = genericShow
instance eqMessage :: Eq Message where
  eq = genericEq
instance ordMessage :: Ord Message where
  compare = genericCompare

-- | A Midi Track.
newtype Track = Track
  (List Message)

derive instance genericTrack :: Generic Track _

instance showTrack :: Show Track where
  show = genericShow

instance eqTrack :: Eq Track where
  eq = genericEq

instance ordTrack :: Ord Track where
  compare = genericCompare


-- | The Midi Header.
newtype Header = Header
    { formatType :: Int
    , trackCount :: Int
    , ticksPerBeat :: Int
    }

derive instance genericHeader :: Generic Header _

instance showHeader :: Show Header where
  show = genericShow

instance eqHeader :: Eq Header where
  eq = genericEq

-- | A Midi Recording.
newtype Recording = Recording
    { header :: Header
    , tracks :: List Track
    }

derive instance genericRecording :: Generic Recording _

instance showRecording :: Show Recording where
  show = genericShow

instance eqRecording :: Eq Recording where
  eq = genericEq
