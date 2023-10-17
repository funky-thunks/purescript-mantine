module Mantine.Core.Prelude
  ( module Data.Default
  , module Data.Either
  , module Data.Maybe
  , module Data.Nullable
  , module Data.Tuple
  , module Data.Tuple.Nested
  , module Effect
  , module Effect.Uncurried
  , module Foreign.Object
  , module Mantine.Core.Common
  , module Mantine.FFI
  , module React.Basic
  , module React.Basic.Events
  , module React.Basic.Hooks
  , module Record
  , module Type.Proxy
  , module Type.Row
  , module Untagged.Union
  ) where

import Data.Default (class DefaultValue, defaultValue)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, notNull, null, toMaybe, toNullable)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Foreign.Object (Object, fromFoldable)
import Mantine.Core.Common
import Mantine.FFI (class FromFFI, class ToFFI, fromNative, toNative)
import React.Basic (ReactComponent, Ref)
import React.Basic.Events (EventHandler, SyntheticEvent, handler, handler_)
import React.Basic.Hooks (JSX)
import Record (delete, merge, rename, union)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Untagged.Union (type (|+|), asOneOf)
