{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "midi"
, dependencies =
  [
  , "effect"
  , "integers"
  , "lists"
  , "prelude"
  , "signal"
  , "string-parsers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
