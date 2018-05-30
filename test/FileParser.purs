module Test.FileParser (parserSuite) where

import Prelude (Unit, bind, discard, show, ($), (<<<), (<>))
import Data.Midi (Recording)
import Data.Midi.Parser (normalise, parse)
import Node.Path as Path
import Effect.Exception (Error)
import Effect.Class (liftEffect)
import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Node.Buffer (toString)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile)
import Test.Unit (Test, TestF, test, suite, success, failure)

parserSuite ::  Free TestF Unit
parserSuite = do
  suite "file parser" do
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
    test "The bonny bonny banks of loch Lomond" do
      assertParses "lomond.midi"

-- | tunnel a binary MIDI file as text and parse it
assertParses :: String -> Test
assertParses fileName =
  do
    let
      fp = Path.concat
    buffer <- readFile (fp ["midi", fileName])
    estr <- liftEffect $ (toString Binary) buffer
    canParse estr

canParse :: String -> Test
canParse str =
  case fullParse str of
    Right _ ->
      success
    Left err ->
      failure (err)
        where
          fullParse :: String -> Either String Recording
          fullParse s =
            (parse <<< normalise) s
