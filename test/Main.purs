module Test.Main where

import Prelude (Unit, discard, ($))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner (runSpec)
import Test.FileParser (parserSpec)
import Test.Instrument (instrumentChecksSpec)
import Test.Parser (parserChecksSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ specReporter] do
  parserSpec
  instrumentChecksSpec
  parserChecksSpec
