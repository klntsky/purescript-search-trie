{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "search-trie"
, dependencies =
  [ "arrays"
  , "assert"
  , "bifunctors"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "lists"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
