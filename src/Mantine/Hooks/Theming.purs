module Mantine.Hooks.Theming
  ( useMantineColorScheme
  , MantineColorScheme (..)
  , UseMantineColorScheme
  ) where

import Prelude (Unit)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import React.Basic.Hooks (type (/\), Hook, unsafeHook)

foreign import data UseMantineColorScheme :: Type -> Type

data MantineColorScheme
  = MantineColorSchemeDark
  | MantineColorSchemeLight

useMantineColorScheme :: Hook UseMantineColorScheme (MantineColorScheme /\ Effect Unit)
useMantineColorScheme =
  let wrap scheme = Tuple (decodeScheme scheme)
      decodeScheme =
        case _ of
          "dark" -> MantineColorSchemeDark
          _      -> MantineColorSchemeLight
   in unsafeHook (
        runEffectFn1 useMantineColorScheme_ (mkFn2 wrap)
      )

foreign import useMantineColorScheme_ :: EffectFn1 (Fn2 String (Effect Unit) (MantineColorScheme /\ Effect Unit)) (MantineColorScheme /\ Effect Unit)
