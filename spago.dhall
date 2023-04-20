{ name = "halogen-project"
, dependencies =
  [ "console"
  , "effect"
  , "prelude"
  , "specular"
  , "tuples"
  , "arrays"
  , "maybe"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
