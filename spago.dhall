{ name = "mantine"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "console"
  , "default"
  , "effect"
  , "either"
  , "exceptions"
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
  , "web-events"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
