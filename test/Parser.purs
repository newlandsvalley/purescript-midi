module Test.Parser (parserChecksSuite) where


import Data.Midi

import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Free (Free)
import Data.Array (singleton)
import Data.Char (fromCharCode)
import Data.Either (Either(..))
import Data.List (List(..), (:), toUnfoldable)
import Data.Midi.Generate as Generate
import Data.Midi.Parser (parse, parseMidiEvent, parseMidiMessage)
import Data.NonEmpty (NonEmpty, (:|))
import Data.String (fromCharArray)
import Prelude (Unit, ($), (<$>), (<*>), (<>), (+), bind, discard, map, pure)
import Test.QuickCheck (Result, (===))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt, listOf, oneOf)
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

-- | we only handle the F0 flavour at the moment
arbSysEx :: Generate.Context -> Gen Event
arbSysEx ctx =
    SysEx F0 <$> arbSysExBytes ctx

commonEvents' :: Array (Gen Event)
commonEvents' =
    [ arbNoteOn
    , arbNoteOff
    , arbNoteAfterTouch
    , arbControlChange
    , arbProgramChange
    , arbPitchBend
    ]

commonEvents :: NonEmpty Array (Gen Event)
commonEvents =
  arbNoteOn :| commonEvents'

allEvents :: NonEmpty Array (Gen Event)
allEvents =
  arbNoteOn :| (commonEvents' <> singleton (arbSysEx Generate.File))

allStreamEvents :: NonEmpty Array (Gen Event)
allStreamEvents =
  arbNoteOn :| (commonEvents' <> singleton (arbSysEx Generate.Stream))

-- | only necessary if we want to test file-based stream events in isolation
arbTestEvent :: Gen TestEvent
arbTestEvent =
  TestEvent <$> oneOf allEvents

arbTestStreamEvent :: Gen TestStreamEvent
arbTestStreamEvent =
  TestStreamEvent <$> oneOf allStreamEvents

arbMessage :: Gen Message
arbMessage =
  Message <$>
    arbDeltaTime <*> (oneOf commonEvents)

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
