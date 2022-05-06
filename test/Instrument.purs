module Test.Instrument (instrumentChecksSpec) where


import Prelude (Unit, ($), (<$>), discard, map)
import Data.List (toUnfoldable)
import Data.Array.NonEmpty (fromNonEmpty)
import Data.NonEmpty ((:|))
import Data.Maybe (Maybe(..))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck (Result(), (===))

import Test.Spec.QuickCheck (quickCheck)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
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
  elements $ fromNonEmpty $
    AcousticGrandPiano :| toUnfoldable instrumentNames

arbGleitzmanName :: Gen String
arbGleitzmanName =
  elements $ fromNonEmpty $
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

instrumentChecksSpec :: Spec Unit
instrumentChecksSpec = do
  describe "instrument" do
    it "handles an unknown instrument name" do
      Nothing `shouldEqual` (readGleitzman "unknown")
    it "round trips instrument names" do
      quickCheck roundTripInstrumentProperty
    it "round trips Gleitzman names" do
      quickCheck roundTripGleitzmanProperty
      