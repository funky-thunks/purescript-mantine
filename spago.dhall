{ name = "mantine"
, dependencies =
  [ "console"
  , "effect"
  , "either"
  , "functions"
  , "maybe"
  , "newtype"
  , "nullable"
  , "prelude"
  , "react-basic"
  , "react-basic-emotion"
  , "react-basic-hooks"
  , "react-icons"
  , "record"
  , "tuples"
  , "typelevel-prelude"
  , "untagged-union"
  , "web-dom"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
