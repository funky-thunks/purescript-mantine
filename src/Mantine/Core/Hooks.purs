module Mantine.Core.Hooks
  ( useMantineColorScheme
  , MantineColorScheme (..)
  , UseMantineColorScheme

  , UseMove
  , UseMovePosition
  , UseMoveHandlers
  , useMove

  , useFocusWithin
  , UseFocusWithin
  ) where

import Prelude
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Nullable (Nullable)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, runEffectFn1, runEffectFn2)
import React.Basic.Hooks (type (/\), Hook, Ref, (/\), unsafeHook)
import Web.DOM (Node)

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

type UseFocusWithinHandlers =
  { onFocus :: Effect Unit
  , onBlur  :: Effect Unit
  }

type UseFocusWithResult =
  { focused :: Boolean
  , ref     :: Ref (Nullable Node)
  }

foreign import useFocusWithinImpl :: EffectFn1 UseFocusWithinHandlers UseFocusWithResult

foreign import data UseFocusWithin :: Type -> Type

useFocusWithin ::  UseFocusWithinHandlers -> Hook UseFocusWithin (Boolean /\ Ref (Nullable Node))
useFocusWithin handlers =
  let mkResult { focused, ref } = focused /\ ref
   in unsafeHook (mkResult <$> runEffectFn1 useFocusWithinImpl handlers)
