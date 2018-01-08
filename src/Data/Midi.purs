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
import Data.Maybe (Maybe)
import Data.Generic.Rep
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)

type Ticks = Int

type Byte = Int

type Channel = Int

type Note = Int

type Velocity = Int

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

-- |  Midi Event
-- |
-- | Note that RunningStatus messages are not included within Event
-- | because the parser translates them to the underlying channel messages
-- |
-- | For TimeSignature a b c d, the basic signature is a/b (for example 3/4)
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
    | SysEx SysExFlavour (List Byte)
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

-- | a timestamped parsed MIDI Event message
newtype TimedEvent = TimedEvent {
    timeStamp :: Number
  , event     :: Maybe Event
  }

derive instance genericTimedEvent:: Generic TimedEvent _

instance showTimedEvent :: Show TimedEvent where
  show = genericShow

-- | Midi Message
data Message = Message Ticks Event

derive instance genericMessage :: Generic Message _
instance showMessage :: Show Message where
  show = genericShow
instance eqMessage :: Eq Message where
  eq = genericEq
instance ordMessage :: Ord Message where
  compare = genericCompare

-- | Midi Track
data Track = Track
  (List Message)

derive instance genericTrack :: Generic Track _

instance showTrack :: Show Track where
  show = genericShow

instance eqTrack :: Eq Track where
  eq = genericEq

instance ordTrack :: Ord Track where
  compare = genericCompare


-- | Midi Header
data Header = Header
    { formatType :: Int
    , trackCount :: Int
    , ticksPerBeat :: Int
    }

derive instance genericHeader :: Generic Header _

instance showHeader :: Show Header where
  show = genericShow

instance eqHeader :: Eq Header where
  eq = genericEq

instance ordHeader :: Ord Header where
  compare = genericCompare

-- | Midi Recording
data Recording = Recording
    { header :: Header
    , tracks :: List Track
    }

derive instance genericRecording :: Generic Recording _

instance showRecording :: Show Recording where
  show = genericShow

instance eqRecording :: Eq Recording where
  eq = genericEq

instance ordRecording:: Ord Recording where
  compare = genericCompare
