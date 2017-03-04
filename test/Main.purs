module Test.Main where

import Prelude
import Data.Midi
import Data.Midi.Parser (normalise, parse)
import Node.Path as Path
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Either (Either(..))
import Data.Char (fromCharCode)
import Data.String (fromCharArray)
import Node.Buffer (BUFFER, toArray, toString)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Async (readFile)

main :: forall e. Eff (console :: CONSOLE, fs :: FS, buffer :: BUFFER | e) Unit
main = do
  parseMidiFile "lillasystern.midi"
  parseMidiFile "frost.midi"
  parseMidiFile "chordsample.midi"
  parseMidiFile "carolansreceipt.midi"
  parseMidiFile "Galway-Hornpipe.midi"
  parseMidiFile "plxburke.midi"

-- | tunnel a binary MIDI file as text and parse it
parseMidiFile :: forall e. String -> Eff (console :: CONSOLE, fs :: FS, buffer :: BUFFER | e) Unit
parseMidiFile fileName =
  let
    fp = Path.concat
  in readFile (fp ["midi", fileName]) $ \x -> do
    case x of
      Left err ->
        log $ "Read error:" <> show err
      Right x' -> do
       {-}
       arr <- toArray x'
       logShow (fullParse $ denormalise arr)
       -}
       str <- (toString Binary) x'
       logShow (fullParse $ str)

{- this is what toString Binary is doing under the covers
denormalise :: Array Int -> String
denormalise is =
  fromCharArray $ map fromCharCode is
-}


fullParse :: String -> String
fullParse s =
  case parse $ normalise $ s of
    Left err ->
      ("Parse error:" <> err)
    Right midi ->
      (show midi)
