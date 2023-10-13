module Mantine.Core.Hooks.Utilities
  ( useDocumentTitle
  , UseDocumentTitle
  , useFavicon
  , UseFavicon
  , useHash
  , UseHash
  , usePageLeave
  , UsePageLeave
  , useWindowEvent
  , UseWindowEvent
  , useWindowScroll
  , UseWindowScroll
  , Position
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1)
import React.Basic.Hooks (type (/\), Hook, (/\), unsafeHook)
import Web.Event.Event (Event)

foreign import useDocumentTitleImpl :: String -> Effect Unit
foreign import data UseDocumentTitle :: Type -> Type

useDocumentTitle :: String -> Hook UseDocumentTitle Unit
useDocumentTitle t = unsafeHook (useDocumentTitleImpl t)

foreign import useFaviconImpl :: String -> Effect Unit
foreign import data UseFavicon :: Type -> Type

useFavicon :: String -> Hook UseFavicon Unit
useFavicon t = unsafeHook (useFaviconImpl t)

foreign import useHashImpl :: Effect { hash :: String, setHash :: EffectFn1 String Unit }
foreign import data UseHash :: Type -> Type

useHash :: Hook UseHash (String /\ (String -> Effect Unit))
useHash =
  let fromNative { hash, setHash } = hash /\ runEffectFn1 setHash
   in unsafeHook (fromNative <$> useHashImpl)

foreign import usePageLeaveImpl :: Effect Unit -> Effect Unit
foreign import data UsePageLeave :: Type -> Type

usePageLeave :: Effect Unit -> Hook UsePageLeave Unit
usePageLeave e = unsafeHook (usePageLeaveImpl e)

type UseWindowEventOptions =
  { type     :: String
  , listener :: Event -> Effect Unit
  }

type UseWindowEventOptionsImpl =
  { type     :: String
  , listener :: EffectFn1 Event Unit
  }

foreign import useWindowEventImpl :: EffectFn1 UseWindowEventOptionsImpl Unit
foreign import data UseWindowEvent :: Type -> Type

useWindowEvent :: UseWindowEventOptions -> Hook UseWindowEvent Unit
useWindowEvent options =
  let nativeOptions = options { listener = mkEffectFn1 options.listener }
   in unsafeHook (runEffectFn1 useWindowEventImpl nativeOptions)

type UseWindowScrollImpl =
  { current :: Position
  , moveTo  :: EffectFn1 Position Unit
  }

type Position =
  { x :: Number
  , y :: Number
  }

foreign import useWindowScrollImpl :: Effect UseWindowScrollImpl
foreign import data UseWindowScroll :: Type -> Type

useWindowScroll :: Hook UseWindowScroll (Position /\ (Position -> Effect Unit))
useWindowScroll =
  let toNative { current, moveTo } = current /\ runEffectFn1 moveTo
   in unsafeHook (toNative <$> useWindowScrollImpl)
