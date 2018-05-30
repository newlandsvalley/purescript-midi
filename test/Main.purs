module Test.Main where

import Prelude (Unit, discard)
import Effect (Effect)
import Test.Unit (suite)
import Test.Unit.Main (runTest)
import Test.FileParser (parserSuite)
import Test.Instrument (instrumentChecksSuite)
import Test.Parser (parserChecksSuite)

main :: Effect Unit
main =
  runTest do
    suite "midi" do
      parserSuite
      instrumentChecksSuite
      parserChecksSuite
