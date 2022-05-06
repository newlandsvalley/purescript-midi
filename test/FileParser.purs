module Test.FileParser (parserSpec) where

import Prelude (Unit, bind, discard, pure, unit, ($), (<<<))
import Control.Monad.Error.Class (class MonadThrow)
import Data.Midi (Recording)
import Data.Midi.Parser (normalise, parse)
import Node.Path as Path
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Data.Either (Either(..))
import Node.Buffer (toString)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

parserSpec ::  Spec Unit
parserSpec = do
  describe "midi" do
    describe "file parser" do
      it "parses lillasystern" do
        assertParses "lillasystern.midi"
      it "parses frost" do
        assertParses "frost.midi"
      it "parses a chord sample" do
        assertParses "chordsample.midi"
      it "parses Carolan's receipt" do
        assertParses "carolansreceipt.midi"
      it "parses the Galway hornpipe" do
        assertParses "Galway-Hornpipe.midi"
      it "parses Planxty Burke" do
        assertParses "plxburke.midi"
      it "parses the bonny banks of Loch Lomond" do
        assertParses "lomond.midi"

-- | tunnel a binary MIDI file as text and parse it
assertParses :: String -> Aff Unit
assertParses fileName =
  do
    let
      fp = Path.concat
    buffer <- readFile (fp ["midi", fileName])
    estr <- liftEffect $ (toString Binary) buffer
    canParse estr

canParse :: forall m. MonadThrow Error m => String -> m Unit
canParse str =
  let
    fullParse :: String -> Either String Recording
    fullParse s =
      (parse <<< normalise) s
  in
    case fullParse str of
      Right _ -> 
        pure unit
      Left err ->
        fail err
