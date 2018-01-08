module Test.Parser (parserChecksSuite) where


import Data.Midi

import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Free (Free)
import Data.Array (singleton)
import Data.Char (fromCharCode)
import Data.Either (Either(..))
import Data.List (List(..), (:), fromFoldable, toUnfoldable)
import Data.Tuple (Tuple(..))
import Data.Midi.Generate as Generate
import Data.Midi.Parser (parse, parseMidiEvent, parseMidiMessage)
import Data.NonEmpty (NonEmpty, (:|))
import Data.String (fromCharArray)
import Prelude (Unit, ($), (<$>), (<*>), (<>), (+), bind, discard, map, negate, pure)
import Test.QuickCheck (Result, (===))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt, elements, frequency, listOf, oneOf)
import Test.Unit (TestF, test, suite)
import Test.Unit.QuickCheck (quickCheck)

-- | quickcheck-style tests adapted from the elm-comidi tests courtesy of @rhofour


-- | we need to wrap our testable data structures in newtypes here because
-- | each is required to be an instance of Arbitrary and the orphan instance
-- | requirement means that we have always to associate the type with the
-- | instance in the same compilation unit.  We don't want to place it in
-- | Data.Midi because it introduces an unnecessary dependency.

-- | When tested in isolation, Events are always StreamEvents
newtype TestStreamEvent = TestStreamEvent Event

instance testStreamEventarb :: Arbitrary TestStreamEvent where
  arbitrary = arbTestStreamEvent

-- | we don't really need to expose Event (always embedded in a file)
-- | unless we ever require to test file event parsing in isolation
newtype TestEvent = TestEvent Event

instance testEventarb :: Arbitrary TestEvent where
  arbitrary = arbTestEvent

newtype TestMessage = TestMessage Message

instance testMessagearb :: Arbitrary TestMessage where
  arbitrary = arbTestMessage

newtype TestRecording = TestRecording Recording

instance testRecordingarb :: Arbitrary TestRecording where
  arbitrary = arbTestRecording

-- generators
arbChannel :: Gen Int
arbChannel = chooseInt 0 15

arbNote :: Gen Int
arbNote = chooseInt 0 127

arbVelocity :: Gen Int
arbVelocity = chooseInt 0 127

arbTrackCount :: Gen Int
arbTrackCount = chooseInt 1 5

-- | multi-track recordings:
-- | type 1 is simultaneous, type 2 is sequentially independent
-- | single tracks must be type 0
arbTrackFormat :: Int -> Gen Int
arbTrackFormat trackCount =
  case trackCount of
    1 ->
      pure 0
    _ ->
      chooseInt 1 2

arbSysExFlavour :: Gen SysExFlavour
arbSysExFlavour =
  elements $
    F0 :| [ F0, F7 ]

-- | because velocity 0 means NoteOff, we don't want to issue
-- | this whilst generating NoteOn
arbPositiveVelocity :: Gen Int
arbPositiveVelocity = chooseInt 1 127

arbControllerNumber :: Gen Int
arbControllerNumber = chooseInt 0 119

-- | generate an arbitrary delta time which will be encoded as a varInt
arbDeltaTime :: Gen Int
arbDeltaTime = chooseInt 0 0x0FFFFFFF

arbSysExByte :: Gen Int
arbSysExByte = chooseInt 0 127

arbByte :: Gen Int
arbByte = chooseInt 0 255

arbUint16 :: Gen Int
arbUint16 = chooseInt 0 0xFFFF

arbUint24 :: Gen Int
arbUint24 = chooseInt 0 0xFFFFFF

arbHour :: Gen Int
arbHour = chooseInt 0 23

arbMinSec :: Gen Int
arbMinSec = chooseInt 0 59

arbFrame :: Gen Int
arbFrame = chooseInt 0 30

arbFrameFraction :: Gen Int
arbFrameFraction = chooseInt 0 99

-- \ 7 flats to 7 sharps
arbAccidentalCount :: Gen Int
arbAccidentalCount = chooseInt (-7) 7

-- \ major minor
arbMode :: Gen Int
arbMode = chooseInt 0 1


-- | the format of a SysEx event differs depending on
-- whether it belongs to a stream or a file
arbSysExBytes :: Generate.Context -> Gen (List Int)
arbSysExBytes ctx =
  do
    count <- chooseInt 2 5
    --  count <- chooseInt 2 127
    bytes <- listOf count arbSysExByte
    let
      terminatedBytes =  (bytes <> (0xF7 : Nil))
      countedBytes = (count + 1) : terminatedBytes
    pure (case ctx of
        Generate.File -> countedBytes
        Generate.Stream -> terminatedBytes
        )

arbBytes :: Gen (List Int)
arbBytes =
  do
    -- count <- chooseInt 1 5
    count <- chooseInt 1 256
    listOf count arbByte

-- arbitrary channel events

arbNoteOn :: Gen Event
arbNoteOn =
  NoteOn <$> arbChannel <*> arbNote <*> arbPositiveVelocity

arbNoteOff :: Gen Event
arbNoteOff =
  NoteOff <$> arbChannel <*> arbNote <*> arbVelocity

arbNoteAfterTouch :: Gen Event
arbNoteAfterTouch =
  NoteAfterTouch <$> arbChannel <*> arbNote <*> arbVelocity

arbControlChange :: Gen Event
arbControlChange =
  ControlChange <$> arbChannel <*> arbControllerNumber <*> arbVelocity

arbProgramChange :: Gen Event
arbProgramChange =
  ProgramChange <$> arbChannel <*> arbVelocity

arbChannelAfterTouch :: Gen Event
arbChannelAfterTouch =
  ChannelAfterTouch <$> arbChannel <*> arbVelocity

arbPitchBend :: Gen Event
arbPitchBend =
  PitchBend <$> arbChannel <*> (chooseInt 0 16383)


-- | arbitrary system exclusive event
arbSysEx :: Generate.Context -> Gen Event
arbSysEx ctx =
    SysEx <$> arbSysExFlavour <*> arbSysExBytes ctx

-- arbitrary meta events
arbSequenceNumber :: Gen Event
arbSequenceNumber =
  SequenceNumber <$> arbUint16

arbText :: Gen Event
arbText =
  Text <$> arbitrary

arbCopyright :: Gen Event
arbCopyright=
  Copyright <$> arbitrary

arbTrackName :: Gen Event
arbTrackName =
  TrackName <$> arbitrary

arbInstrumentName :: Gen Event
arbInstrumentName =
  InstrumentName <$> arbitrary

arbLyrics :: Gen Event
arbLyrics =
  Lyrics <$> arbitrary

arbMarker :: Gen Event
arbMarker =
  Marker <$> arbitrary

arbCuePoint :: Gen Event
arbCuePoint =
  CuePoint <$> arbitrary

arbChannelPrefix :: Gen Event
arbChannelPrefix =
  ChannelPrefix <$> arbChannel

arbTempo :: Gen Event
arbTempo =
  Tempo <$> arbUint24

arbSMPTEOffset :: Gen Event
arbSMPTEOffset =
  SMPTEOffset <$> arbHour <*> arbMinSec <*> arbMinSec <*> arbFrame <*> arbFrameFraction

arbKeySignature :: Gen Event
arbKeySignature =
  KeySignature <$> arbAccidentalCount <*> arbMode

-- very arbitrary arbitraries!
arbTimeSignature :: Gen Event
arbTimeSignature =
  TimeSignature
    <$>  (chooseInt 1 100)  -- numerator
    <*>  elements (4 :| [1,2,4,8,16,32,64,128 ]) -- denominator
    <*>  (pure 24)          -- clock count
    <*>  (chooseInt 1 64)   -- 32nd notes per quarter note

arbSequencerSpecific  :: Gen Event
arbSequencerSpecific =
  SequencerSpecific <$> arbBytes

-- tiny example - needs expanding
arbUnspecified :: Gen Event
arbUnspecified =
  Unspecified <$> (pure 0x60) <*> arbBytes


channelEvents :: Array (Gen Event)
channelEvents =
    [ arbNoteOn
    , arbNoteOff
    , arbNoteAfterTouch
    , arbControlChange
    , arbProgramChange
    , arbPitchBend
    ]

weightedChannelEvents :: List (Tuple Number (Gen Event))
weightedChannelEvents =
    ( Tuple 200.0 arbNoteOn
    : Tuple 200.0 arbNoteOff
    : Tuple 10.0 arbNoteAfterTouch
    : Tuple 10.0 arbControlChange
    : Tuple 5.0 arbProgramChange
    : Tuple 5.0 arbPitchBend
    : Nil
    )


metaEvents :: Array (Gen Event)
metaEvents =
  [ arbSequenceNumber
  , arbText
  , arbCopyright
  , arbTrackName
  , arbInstrumentName
  , arbLyrics
  , arbMarker
  , arbCuePoint
  , arbTempo
  , arbSMPTEOffset
  , arbTimeSignature
  , arbKeySignature
  , arbSequencerSpecific
  , arbUnspecified
  ]


weightedMetaEvents :: List (Tuple Number (Gen Event))
weightedMetaEvents =
  fromFoldable $ map (Tuple 1.0) metaEvents

weightedSysExEvent :: Generate.Context -> Tuple Number (Gen Event)
weightedSysExEvent ctx =
  Tuple 20.0 $ arbSysEx ctx

commonEvents :: NonEmpty Array (Gen Event)
commonEvents =
  arbNoteOn :| channelEvents

allEvents :: Generate.Context -> NonEmpty Array (Gen Event)
allEvents ctx =
  arbNoteOn :| (channelEvents <> singleton (arbSysEx ctx) <> metaEvents)

weightedEvents :: Generate.Context -> NonEmpty List (Tuple Number (Gen Event))
weightedEvents ctx =
  (Tuple 1.0 arbNoteOn)
     :|
       ( weightedChannelEvents
         <> ( (weightedSysExEvent ctx) : Nil)
         <> weightedMetaEvents
        )

-- | only necessary if we want to test file-based stream events in isolation
arbTestEvent :: Gen TestEvent
arbTestEvent =
  TestEvent <$> oneOf (allEvents Generate.File)

arbTestStreamEvent :: Gen TestStreamEvent
arbTestStreamEvent =
  TestStreamEvent <$> frequency (weightedEvents Generate.Stream)
  -- TestStreamEvent <$> oneOf (allEvents Generate.Stream)

arbMessage :: Gen Message
arbMessage =
  Message <$>
    arbDeltaTime <*> frequency (weightedEvents Generate.File)
    --  arbDeltaTime <*> (oneOf allEvents)

arbTestMessage :: Gen TestMessage
arbTestMessage =
  TestMessage <$> arbMessage

arbTrack :: Gen Track
arbTrack =
  do
    count <- chooseInt 0 250
    Track <$> listOf count arbMessage

arbTracks :: Int -> Gen (List Track)
arbTracks trackCount =
  listOf trackCount arbTrack

arbHeader :: Int -> Gen Header
arbHeader trackCount =
  do
    formatType <- arbTrackFormat trackCount
    pure
      (Header
        { formatType : formatType
        , trackCount : trackCount
        , ticksPerBeat : 440
        }
      )

arbRecording :: Gen Recording
arbRecording =
    do
      trackCount <- arbTrackCount
      header <- arbHeader trackCount
      tracks <- arbTracks trackCount
      pure
        (Recording
          { header : header
          , tracks : tracks
          }
        )

arbTestRecording :: Gen TestRecording
arbTestRecording =
  TestRecording <$> arbRecording

toByteString :: List Int -> String
toByteString list =
    fromCharArray $ map fromCharCode (toUnfoldable list)

-- | the test properties

roundTripStreamEventProperty :: TestStreamEvent -> Result
roundTripStreamEventProperty (TestStreamEvent e) =
  let
    event = parseMidiEvent $ toByteString $ Generate.event Generate.Stream e
  in
    (Right e :: Either String Event) === event

roundTripMessageProperty :: TestMessage -> Result
roundTripMessageProperty (TestMessage m) =
  let
    message = parseMidiMessage $ toByteString $ Generate.midiMessage m
  in
    (Right m :: Either String Message) === message

roundTripRecordingProperty :: TestRecording -> Result
roundTripRecordingProperty (TestRecording r) =
  let
    recording = parse $ toByteString $ Generate.recording r
  in
    (Right r :: Either String Recording) === recording

-- | the test suite
parserChecksSuite :: forall t. Free (TestF (random :: RANDOM | t)) Unit
parserChecksSuite = do
  suite "parser" do
    test "round trip (stream) event" do
      quickCheck roundTripStreamEventProperty
    test "round trip message" do
      quickCheck roundTripMessageProperty
    test "round trip recording" do
      quickCheck roundTripRecordingProperty