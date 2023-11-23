{ name = "mantine"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "console"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "functions"
  , "integers"
  , "js-date"
  , "maybe"
  , "naturals"
  , "newtype"
  , "nullable"
  , "numbers"
  , "prelude"
  , "react-basic"
  , "react-basic-dom"
  , "react-basic-emotion"
  , "react-basic-hooks"
  , "react-icons"
  , "record"
  , "strings"
  , "tuples"
  , "typelevel-prelude"
  , "unsafe-coerce"
  , "untagged-union"
  , "web-dom"
  , "web-events"
  , "web-file"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
