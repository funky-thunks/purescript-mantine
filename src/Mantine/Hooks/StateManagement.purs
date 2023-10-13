module Mantine.Hooks.StateManagement
  ( useIdle
  , UseIdle
  ) where

import Effect.Uncurried (EffectFn1, runEffectFn1)
import React.Basic.Hooks (Hook, unsafeHook)

foreign import useIdleImpl :: EffectFn1 Number Boolean

foreign import data UseIdle :: Type -> Type

useIdle :: Number -> Hook UseIdle Boolean
useIdle n = unsafeHook (runEffectFn1 useIdleImpl n)
