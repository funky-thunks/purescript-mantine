{ name = "mantine"
, dependencies =
  [ "console"
  , "effect"
  , "either"
  , "maybe"
  , "newtype"
  , "nullable"
  , "prelude"
  , "react-basic"
  , "react-basic-emotion"
  , "record"
  , "typelevel-prelude"
  , "untagged-union"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
