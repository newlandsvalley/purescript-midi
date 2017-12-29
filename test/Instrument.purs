module Test.Instrument (instrumentChecksSuite) where


import Prelude (Unit, ($), (<$>), discard)
import Data.List (toUnfoldable)
import Data.NonEmpty ((:|))
import Data.Maybe (Maybe(..))
import Control.Monad.Free (Free)
import Control.Monad.Eff.Random (RANDOM)
import Test.Unit (TestF, test, suite)
import Test.Unit.Assert (equal) as Assert
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck (Result(), (===))
import Test.Unit.QuickCheck (quickCheck)
import Test.QuickCheck.Gen (Gen, elements)
import Data.Midi.Instrument (InstrumentName(..), gleitzmanName, instrumentNames, read)

newtype TestInstrument = TestInstrument InstrumentName

instance testInstrumentarb :: Arbitrary TestInstrument where
  arbitrary = arbTestInstrument

arbInstrumentName :: Gen InstrumentName
arbInstrumentName =
  elements $
    AcousticGrandPiano :| toUnfoldable instrumentNames

arbTestInstrument :: Gen TestInstrument
arbTestInstrument =
  TestInstrument <$> arbInstrumentName

roundTripInstrumentProperty :: TestInstrument -> Result
roundTripInstrumentProperty (TestInstrument i) =
  let
    instrument = read $ gleitzmanName i
  in
    (Just i :: Maybe InstrumentName) === instrument

instrumentChecksSuite :: forall t. Free (TestF (random :: RANDOM | t)) Unit
instrumentChecksSuite = do
  suite "instrument" do
    test "unknown" do
      Assert.equal Nothing (read "unknown")
    test "round trip instrument name" do
      quickCheck roundTripInstrumentProperty
