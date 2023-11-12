# Purescript bindings for mantine

Bindings to use [Mantine](https://mantine.dev) components from a Purescript codebase.

Currently supporting mantine v7.2.1.

:warning: This is work-in-progress and this is very unstable.

## Install

Edit your `packages.dhall` to add purescript-mantine and some of its dependencies to your package-set:

```dhall
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.10-20230906/packages.dhall
        sha256:7c5742a4c04391aa043be617c04ffcd478f2fc6cbc00c41ef9558ef4bcc4c355

in  upstream
      with default =
        { dependencies =
            [ "prelude"
            , "either"
            , "maybe"
            , "tuples"
            , "lists"
            , "ordered-collections"
            , "foreign-object"
            ]
        , repo = "https://github.com/imsaravana369/purescript-default"
        , version = "v1.0.1"
        }
      with mantine =
        { dependencies =
            [ "aff"
            , "aff-promise"
            , "console"
            , "contravariant"
            , "default"
            , "effect"
            , "either"
            , "exceptions"
            , "foldable-traversable"
            , "foreign"
            , "foreign-object"
            , "functions"
            , "integers"
            , "maybe"
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
        , repo =
            "https://github.com/funky-thunks/purescript-mantine"
        , version = "8c20f15093fe9e1a8cf07d4ad926a4f09d283b7d"
        }
```

```bash
npm install --save @mantine/core@7.2.1 @mantine/hooks@7.2.1 @mantine/dates@7.2.1 dayjs react
spago install mantine
```
