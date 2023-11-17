module Mantine.Hooks.Prelude
  ( module Control.Promise
  , module Data.Either
  , module Data.Function.Uncurried
  , module Data.Maybe
  , module Data.Nullable
  , module Data.Tuple
  , module Effect
  , module Effect.Aff
  , module Effect.Exception
  , module Effect.Uncurried
  , module Mantine.FFI
  , module Prelude
  , module React.Basic.Hooks
  , module Record
  , module Type.Proxy
  , module Web.DOM
  , module Web.Event.Event
  , module Web.UIEvent.KeyboardEvent

  , mkHook0
  , mkHook1
  , mkHook2
  ) where

import Control.Promise (Promise)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, runEffectFn1, runEffectFn2)
import Mantine.FFI (class FromFFI, class ToFFI, Optional(..), OptionalImpl, toOptionalImpl, fromNative, toNative)
import Prelude
import React.Basic.Hooks (type (/\), Hook, Ref, Render, (/\), unsafeHook)
import Record (delete, merge, union)
import Type.Proxy (Proxy(..))
import Web.DOM (Node)
import Web.Event.Event (Event)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

mkHook0 :: forall hooks newHook result_js result_ps
         . FromFFI result_js result_ps
        => Effect result_js
        -> Render hooks (newHook hooks) result_ps
mkHook0 hookImpl = unsafeHook (fromNative <$> hookImpl)

mkHook1 :: forall hooks newHook result_js result_ps arg1_js arg1_ps
         . FromFFI result_js result_ps
        => ToFFI arg1_ps arg1_js
        => EffectFn1 arg1_js result_js
        -> arg1_ps
        -> Render hooks (newHook hooks) result_ps
mkHook1 hookImpl arg1 = unsafeHook (fromNative <$> runEffectFn1 hookImpl (toNative arg1))

mkHook2 :: forall hooks newHook result_js result_ps arg1_js arg2_js arg1_ps arg2_ps
         . FromFFI result_js result_ps
        => ToFFI arg1_ps arg1_js
        => ToFFI arg2_ps arg2_js
        => EffectFn2 arg1_js arg2_js result_js
        -> arg1_ps
        -> arg2_ps
        -> Render hooks (newHook hooks) result_ps
mkHook2 hookImpl arg1 arg2 = unsafeHook (fromNative <$> runEffectFn2 hookImpl (toNative arg1) (toNative arg2))
