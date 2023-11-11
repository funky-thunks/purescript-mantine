module Mantine.Hooks.StateManagement
  ( useIdle
  , UseIdle
  ) where

import Mantine.Hooks.Prelude

foreign import useIdleImpl :: EffectFn1 Number Boolean

foreign import data UseIdle :: Type -> Type

useIdle :: Number -> Hook UseIdle Boolean
useIdle = mkHook1 useIdleImpl
