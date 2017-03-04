module Test.Main where

import Prelude
import Data.Midi
import Data.Midi.Parser (normalise, parse, translateRunningStatus)
import Node.Path as Path
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..), isRight)
import Node.Buffer (BUFFER, toString)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Async (readFile)
import Test.Assert (ASSERT, assert)

main :: forall e. Eff (buffer :: BUFFER, assert :: ASSERT, console :: CONSOLE, fs :: FS | e) Unit
main = do
  parseMidiFile "lillasystern.midi"
  parseMidiFile "frost.midi"
  parseMidiFile "chordsample.midi"
  parseMidiFile "carolansreceipt.midi"
  parseMidiFile "Galway-Hornpipe.midi"
  parseMidiFile "plxburke.midi"

-- | tunnel a binary MIDI file as text and parse it
parseMidiFile :: forall e. String -> Eff (buffer :: BUFFER, assert :: ASSERT, console :: CONSOLE, fs :: FS | e) Unit
parseMidiFile fileName =
  let
    fp = Path.concat
  in readFile (fp ["midi", fileName]) $ \x -> do
    case x of
      Left err ->
        log $ "Read error:" <> show err
      Right x' -> do
       str <- (toString Binary) x'
       assert $ canParse str

canParse :: String -> Boolean
canParse input = isRight $ fullParse input
  where
    fullParse :: String -> Either String MidiRecording
    fullParse s =
       translateRunningStatus $ parse $ normalise s
