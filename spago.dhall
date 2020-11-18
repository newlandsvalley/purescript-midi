{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "midi"
, dependencies =
  [ "assert"
  , "console"
  , "effect"
  , "generics-rep"
  , "integers"
  , "lists"
  , "node-buffer"
  , "node-fs-aff"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  , "signal"
  , "string-parsers"
  , "test-unit"
  , "unicode"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
