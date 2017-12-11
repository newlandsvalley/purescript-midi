module Test.Main where

import Prelude (Unit, bind, discard, show, ($), (<<<), (<>))
import Data.Midi (Recording)
import Data.Midi.Parser (normalise, parse)
import Data.Midi.Instrument
import Node.Path as Path
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff (liftEff')
import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Node.Buffer (BUFFER, toString)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Aff (readFile)
import Test.Unit.Assert (equal) as Assert
import Test.Unit (Test, TestF, test, suite, success, failure)
import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)

main :: forall t.
        Eff
          ( console :: CONSOLE
          , testOutput :: TESTOUTPUT
          , avar :: AVAR
          , buffer :: BUFFER
          , fs :: FS
          | t
          )
          Unit
main =
  runTest do
    suite "midi" do
      parserSuite
      instrumentSuite

parserSuite :: forall t.
        Free
          (TestF
             ( fs :: FS
             , buffer :: BUFFER
             | t
             )
          )
          Unit
parserSuite = do
  suite "parser" do
    test "lillasystern" do
      assertParses "lillasystern.midi"
    test "frost" do
      assertParses "frost.midi"
    test "chord sample" do
      assertParses "chordsample.midi"
    test "Carolan's receipt" do
      assertParses "carolansreceipt.midi"
    test "Galway hornpipe" do
      assertParses "Galway-Hornpipe.midi"
    test "Planxty Burke" do
      assertParses "plxburke.midi"

instrumentSuite :: forall t. Free (TestF t) Unit
instrumentSuite = do
  suite "instrument" do
    test "Marimba" do
      Assert.equal "marimba" (gleitzmanName Marimba)
    test "AcousticGrandPiano" do
      Assert.equal "acoustic_grand_piano" (gleitzmanName AcousticGrandPiano)
    test "Lead2Sawtooth" do
      Assert.equal "lead_2_sawtooth" (gleitzmanName Lead2Sawtooth)
    test "SynthBass1" do
      Assert.equal "synth_bass_1" (gleitzmanName SynthBass1)
    test "acoustic_grand_piano" do
      Assert.equal (Just AcousticGrandPiano) (read "acoustic_grand_piano")
    test "unknown" do
      Assert.equal Nothing (read "unknown")

-- | tunnel a binary MIDI file as text and parse it
assertParses :: ∀ e.
        String
        -> Test
             ( fs :: FS
             , buffer :: BUFFER
             | e
             )
assertParses fileName =
  do
    let
      fp = Path.concat
    buffer <- readFile (fp ["midi", fileName])
    estr <- liftEff' $ (toString Binary) buffer
    canParse estr

canParse :: ∀ e. Either Error String -> Test e
canParse estr =
  case estr of
    Right midi ->
      case fullParse midi of
        Right _ ->
          success
        Left err ->
          failure (err)
    Left err ->
      failure $ "Read error:" <> show err
        where
          fullParse :: String -> Either String Recording
          fullParse s =
            (parse <<< normalise) s
