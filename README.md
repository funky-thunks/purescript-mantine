# Purescript bindings for mantine

Bindings to use [Mantine](https://mantine.dev) components from a Purescript codebase.

Currently supporting mantine v7.2.1.

:warning: This is work-in-progress and this is very unstable.

## Install

Edit your `packages.dhall` to add purescript-mantine and some of its dependencies to your package-set:

```dhall
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.10-20231023/packages.dhall
        sha256:b9a482e743055ba8f2d65b08a88cd772b59c6e2084d0e5ad854025fa90417fd4

in  upstream
  with mantine =
    { dependencies =
      [ "aff"
      , "aff-promise"
      , "console"
      , "contravariant"
      , "default-values"
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
      , "untagged-union"
      , "web-dom"
      , "web-events"
      , "web-file"
      , "web-html"
      , "web-uievents"
      ]
    , repo = "https://github.com/funky-thunks/purescript-mantine"
    , version = "765d0aef223d0385040fc9e05c50e5a7c45a0130"
    }
```

```bash
npm install --save @mantine/core@7.2.1 @mantine/hooks@7.2.1 @mantine/dates@7.2.1 dayjs react
spago install mantine
```
