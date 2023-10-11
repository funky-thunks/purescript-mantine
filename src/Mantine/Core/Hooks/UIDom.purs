module Mantine.Core.Hooks.UIDom
  ( useFocusWithin
  , UseFocusWithin

  , UseMove
  , UseMovePosition
  , UseMoveHandlers
  , useMove

  , useViewportSize
  , UseViewportSize
  , ViewportDimensions
  ) where

import Prelude
import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, runEffectFn1, runEffectFn2)
import React.Basic.Hooks (type (/\), Hook, Ref, (/\), unsafeHook)
import Web.DOM (Node)

type UseFocusWithinHandlers =
  { onFocus :: Effect Unit
  , onBlur  :: Effect Unit
  }

type UseFocusWithinResult =
  { focused :: Boolean
  , ref     :: Ref (Nullable Node)
  }

foreign import useFocusWithinImpl :: EffectFn1 UseFocusWithinHandlers UseFocusWithinResult

foreign import data UseFocusWithin :: Type -> Type

useFocusWithin ::  UseFocusWithinHandlers -> Hook UseFocusWithin (Boolean /\ Ref (Nullable Node))
useFocusWithin handlers =
  let mkResult { focused, ref } = focused /\ ref
   in unsafeHook (mkResult <$> runEffectFn1 useFocusWithinImpl handlers)

type UseMovePosition =
  { x :: Number
  , y :: Number
  }

type UseMoveHandlers =
  { onScrubStart :: Effect Unit
  , onScrubEnd   :: Effect Unit
  }

type UseMoveResult =
  { active :: Boolean
  , ref    :: Ref (Nullable Node)
  }

foreign import useMoveImpl :: EffectFn2 (EffectFn1 UseMovePosition Unit) UseMoveHandlers UseMoveResult

foreign import data UseMove :: Type -> Type

useMove :: (UseMovePosition -> Effect Unit) -> UseMoveHandlers -> Hook UseMove (Boolean /\ Ref (Nullable Node))
useMove onChange handlers =
  let mkResult { active, ref } = active /\ ref
   in unsafeHook (mkResult <$> runEffectFn2 useMoveImpl (mkEffectFn1 onChange) handlers)

type ViewportDimensions =
  { height :: Number
  , width  :: Number
  }

foreign import useViewportSizeImpl :: Effect ViewportDimensions

foreign import data UseViewportSize :: Type -> Type

useViewportSize :: Hook UseViewportSize ViewportDimensions
useViewportSize = unsafeHook  useViewportSizeImpl
