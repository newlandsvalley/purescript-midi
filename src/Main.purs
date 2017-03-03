module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Either (either)
import Data.Midi
import MidiParser (normalise, parse)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  -- log "Hello, sailor!"
  let
    nonMidi = normalise "this is not MIDI"
    result = either show show $ parse nonMidi
  logShow result
