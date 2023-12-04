module Mantine.Hooks.Theming
  ( useMantineColorScheme
  , MantineColorScheme (..)
  , UseMantineColorScheme

  , MantineColorSchemeImpl
  ) where

import Mantine.Hooks.Prelude

foreign import data UseMantineColorScheme :: Type -> Type

data MantineColorScheme
  = MantineColorSchemeDark
  | MantineColorSchemeLight

type MantineColorSchemeImpl = String

instance ToFFI MantineColorScheme MantineColorSchemeImpl where
  toNative =  case _ of
    MantineColorSchemeDark  -> "dark"
    MantineColorSchemeLight -> "light"

instance FromFFI MantineColorSchemeImpl MantineColorScheme where
  fromNative = case _ of
    "dark" -> MantineColorSchemeDark
    _      -> MantineColorSchemeLight

useMantineColorScheme :: UseColorSchemeOptions -> Hook UseMantineColorScheme (MantineColorScheme /\ Effect Unit)
useMantineColorScheme options =
  let picker = mkFn2 (Tuple <<< fromNative)
   in unsafeHook (runEffectFn2 useMantineColorScheme_ (toNative options) picker)

type ColorSchemePicker = Fn2 String (Effect Unit) (MantineColorScheme /\ Effect Unit)

foreign import useMantineColorScheme_ :: EffectFn2 UseColorSchemeOptionsImpl ColorSchemePicker (MantineColorScheme /\ Effect Unit)

type UseColorSchemeOptions     = { keepTransition :: Optional     Boolean }
type UseColorSchemeOptionsImpl = { keepTransition :: OptionalImpl Boolean }
