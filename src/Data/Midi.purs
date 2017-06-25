module Data.Midi
        ( Track(..)
        , Header(..)
        , Event(..)
        , Message(..)
        , Recording(..)
        , Ticks
        ) where

import Prelude (class Show, class Eq, class Ord)
import Data.List (List)
import Data.Generic.Rep
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)

type Ticks =
    Int

-- |  Midi Event
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

derive instance genericEvent :: Generic Event _
instance showEvent :: Show Event where
  show = genericShow
instance eqEvent :: Eq Event where
  eq = genericEq
instance ordEvent :: Ord Event where
  compare = genericCompare

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
