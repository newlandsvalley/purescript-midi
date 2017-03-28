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
import Data.Newtype (class Newtype)
import Data.Generic (gShow, class Generic)

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

derive instance genericEvent :: Generic Event
instance showEvent :: Show Event where
  show = gShow

-- | Midi Message
data Message = Message Ticks Event

derive instance genericMessage :: Generic Message
instance showMessage :: Show Message where
  show = gShow


-- | Midi Track
newtype Track = Track
  (List Message)

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
newtype Recording = Recording
    { header :: Header
    , tracks :: List Track
    }


derive instance newtypeRecording :: Newtype Recording _
derive instance genericRecording :: Generic Recording
instance showRecording :: Show Recording where
  show = gShow
