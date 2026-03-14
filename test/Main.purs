module Test.Main where

import Prelude (Unit, discard)
import Effect (Effect)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.FileParser (parserSpec)
import Test.Instrument (instrumentChecksSpec)
import Test.Parser (parserChecksSpec)

main :: Effect Unit
main = runSpecAndExitProcess [ specReporter] do
  parserSpec
  instrumentChecksSpec
  parserChecksSpec
