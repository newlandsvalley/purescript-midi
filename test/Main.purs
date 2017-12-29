module Test.Main where

import Prelude (Unit, discard)
import Node.Buffer (BUFFER)
import Node.FS (FS)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Aff.AVar (AVAR)
import Test.Unit (suite)
import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)
import Test.FileParser (parserSuite)
import Test.Instrument (instrumentSuite)
import Test.Parser (parserChecksSuite)

main :: forall t.
        Eff
          ( console :: CONSOLE
          , testOutput :: TESTOUTPUT
          , avar :: AVAR
          , buffer :: BUFFER
          , fs :: FS
          , random :: RANDOM
          | t
          )
          Unit
main =
  runTest do
    suite "midi" do
      parserSuite
      instrumentSuite
      parserChecksSuite
