module Test.Instrument (instrumentChecksSuite) where


import Prelude (Unit, ($), (<$>), discard, map)
import Data.List (toUnfoldable)
import Data.NonEmpty ((:|))
import Data.Maybe (Maybe(..))
import Control.Monad.Free (Free)
import Test.Unit (TestF, test, suite)
import Test.Unit.Assert (equal) as Assert
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck (Result(), (===))
import Test.Unit.QuickCheck (quickCheck)
import Test.QuickCheck.Gen (Gen, elements)
import Data.Midi.Instrument (InstrumentName(..), gleitzmanName, gleitzmanNames,
    instrumentNames, readGleitzman)

newtype TestInstrument = TestInstrument InstrumentName

instance testInstrumentarb :: Arbitrary TestInstrument where
  arbitrary = arbTestInstrument

newtype TestGleitzman = TestGleitzman String

instance testGleitzmanarb :: Arbitrary TestGleitzman where
  arbitrary = arbTestGleitzmanName

arbInstrumentName :: Gen InstrumentName
arbInstrumentName =
  elements $
    AcousticGrandPiano :| toUnfoldable instrumentNames

arbGleitzmanName :: Gen String
arbGleitzmanName =
  elements $
    "acoustic_grand_piano" :| toUnfoldable gleitzmanNames

arbTestInstrument :: Gen TestInstrument
arbTestInstrument =
  TestInstrument <$> arbInstrumentName

arbTestGleitzmanName :: Gen TestGleitzman
arbTestGleitzmanName =
  TestGleitzman <$> arbGleitzmanName

roundTripInstrumentProperty :: TestInstrument -> Result
roundTripInstrumentProperty (TestInstrument i) =
  let
    instrument = readGleitzman $ gleitzmanName i
  in
    (Just i :: Maybe InstrumentName) === instrument

roundTripGleitzmanProperty :: TestGleitzman -> Result
roundTripGleitzmanProperty (TestGleitzman s) =
  let
    name = map gleitzmanName $ readGleitzman s
  in
    (Just s :: Maybe String) === name

instrumentChecksSuite :: Free TestF Unit
instrumentChecksSuite = do
  suite "instrument" do
    test "unknown" do
      Assert.equal Nothing (readGleitzman "unknown")
    test "round trip instrument name" do
      quickCheck roundTripInstrumentProperty
    test "round trip Gleitzman name" do
      quickCheck roundTripGleitzmanProperty
