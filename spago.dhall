{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "midi"
, dependencies =
  [ "arrays"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "signal"
  , "string-parsers"
  , "strings"
  , "tuples"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
