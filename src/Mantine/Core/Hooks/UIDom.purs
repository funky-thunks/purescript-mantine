module Mantine.Core.Hooks.UIDom
  ( useFocusWithin
  , UseFocusWithin

  , useFullscreen
  , UseFullscreen
  , UseFullscreenResult

  , useMediaQuery
  , UseMediaQuery
  , UseMediaQueryOptions

  , UseMove
  , UseMovePosition
  , UseMoveHandlers
  , useMove

  , useViewportSize
  , UseViewportSize
  , ViewportDimensions
  ) where

import Prelude
import Control.Promise (Promise, toAff)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Aff (Aff)
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

type UseFullscreenResultImpl =
  { fullscreen :: Boolean
  , ref        :: Ref (Nullable Node)
  , toggle     :: Promise Unit
  }

type UseFullscreenResult =
  { fullscreen :: Boolean
  , ref        :: Ref (Nullable Node)
  , toggle     :: Aff Unit
  }

fromUseFullscreenResultImpl :: UseFullscreenResultImpl -> UseFullscreenResult
fromUseFullscreenResultImpl n = n { toggle = toAff n.toggle }

foreign import useFullscreenImpl :: Effect UseFullscreenResultImpl

foreign import data UseFullscreen :: Type -> Type

useFullscreen :: Hook UseFullscreen UseFullscreenResult
useFullscreen = unsafeHook (fromUseFullscreenResultImpl <$> useFullscreenImpl)

type UseMediaQueryOptions =
  { query        :: String
  , initialValue :: Maybe Boolean
  , options      :: Maybe { getInitialValueInEffect :: Boolean }
  }

type UseMediaQueryOptionsImpl =
  { query        :: String
  , initialValue :: Nullable Boolean
  , options      :: Nullable { getInitialValueInEffect :: Boolean }
  }

foreign import useMediaQueryImpl :: EffectFn1 UseMediaQueryOptionsImpl Boolean

foreign import data UseMediaQuery :: Type -> Type

useMediaQuery :: UseMediaQueryOptions -> Hook UseMediaQuery Boolean
useMediaQuery options =
  let nativeOptions = options { initialValue = toNullable options.initialValue, options = toNullable options.options }
   in unsafeHook (runEffectFn1 useMediaQueryImpl nativeOptions)

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
