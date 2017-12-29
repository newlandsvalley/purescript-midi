module Test.Parser (parserChecksSuite) where


import Prelude (Unit, ($), (<$>), (<*>), discard, map)
import Data.List (List, toUnfoldable)
import Data.Either (Either(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.String (fromCharArray)
import Data.Char (fromCharCode)
import Control.Monad.Free (Free)
import Control.Monad.Eff.Random (RANDOM)
import Test.Unit (TestF, test, suite)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck (Result(), (===))
import Test.Unit.QuickCheck (quickCheck)
import Test.QuickCheck.Gen (Gen, chooseInt, oneOf)
import Data.Midi
import Data.Midi.Generate as Generate
import Data.Midi.Parser (parseMidiEvent, parseMidiMessage)

-- | quickcheck-style tests adapted from the elm-comidi tests courtesy of @rhofour


-- | we need to wrap our testable data structures in newtypes here because
-- | each is required to be an instance of Arbitrary and the orphan instance
-- | requirement means that we have always to associate the type with the
-- | instance in the same compilation unit.  We don't want to place it in
-- | Data.Midi because it introduces an unnecessary dependency.
newtype TestEvent = TestEvent Event

instance testEventarb :: Arbitrary TestEvent where
  arbitrary = arbTestEvent


newtype TestMessage = TestMessage Message

instance testMessagearb :: Arbitrary TestMessage where
  arbitrary = arbTestMessage

-- generators
arbChannel :: Gen Int
arbChannel = chooseInt 0 15

arbNote :: Gen Int
arbNote = chooseInt 0 127

arbVelocity :: Gen Int
arbVelocity = chooseInt 0 127

-- | because velocity 0 means NoteOff, we don't want to issue
-- | this whilst generating NoteOn
arbPositiveVelocity :: Gen Int
arbPositiveVelocity = chooseInt 1 127

arbControllerNumber :: Gen Int
arbControllerNumber = chooseInt 0 119

-- | generate an arbitrary delta time which will be encoded as a varInt
arbDeltaTime :: Gen Int
arbDeltaTime = chooseInt 0 0x0FFFFFFF

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

commonEvents :: NonEmpty Array (Gen Event)
commonEvents =
  arbNoteOn :|
    [ arbNoteOn
    , arbNoteOff
    , arbNoteAfterTouch
    , arbControlChange
    , arbProgramChange
    , arbPitchBend
    ]

arbTestEvent :: Gen TestEvent
arbTestEvent =
  TestEvent <$> oneOf commonEvents

arbTestMessage :: Gen TestMessage
arbTestMessage =
  TestMessage <$> (Message <$>
     arbDeltaTime <*> (oneOf commonEvents))

toByteString :: List Int -> String
toByteString list =
    fromCharArray $ map fromCharCode (toUnfoldable list)

roundTripEventProperty :: TestEvent -> Result
roundTripEventProperty (TestEvent e) =
  let
    pme = parseMidiEvent $ toByteString $ Generate.event e
  in
    (Right e :: Either String Event) === pme

roundTripMessageProperty :: TestMessage -> Result
roundTripMessageProperty (TestMessage m) =
  let
    pmm = parseMidiMessage $ toByteString $ Generate.midiMessage m
  in
    (Right m :: Either String Message) === pmm


parserChecksSuite :: forall t. Free (TestF (random :: RANDOM | t)) Unit
parserChecksSuite = do
  suite "parser" do
    test "round trip event" do
      quickCheck roundTripEventProperty
    test "round trip message" do
      quickCheck roundTripMessageProperty
