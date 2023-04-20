let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20220816/packages.dhall
        sha256:8b4467b4b5041914f9b765779c8936d6d4c230b1f60eb64f6269c71812fd7e98

in  upstream
  with specular =
    { dependencies =
      [ "prelude"
      , "aff"
      , "typelevel-prelude"
      , "record"
      , "unsafe-reference"
      , "random"
      , "debug"
      , "foreign-object"
      , "contravariant"
      , "avar"
      ]
    , repo = "https://github.com/restaumatic/purescript-specular.git"
    , version = "master"
    }
