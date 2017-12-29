module Test.FileParser (parserSuite) where

import Prelude (Unit, bind, discard, show, ($), (<<<), (<>))
import Data.Midi (Recording)
import Data.Midi.Parser (normalise, parse)
import Node.Path as Path
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Aff (liftEff')
import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Node.Buffer (BUFFER, toString)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Aff (readFile)
import Test.Unit (Test, TestF, test, suite, success, failure)

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
