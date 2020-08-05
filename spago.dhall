{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "affjax"
  , "argonaut-codecs"
  , "console"
  , "effect"
  , "halogen"
  , "halogen-hooks"
  , "html-parser-halogen"
  , "psci-support"
  , "random"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
