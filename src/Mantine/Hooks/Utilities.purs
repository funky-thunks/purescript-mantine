module Mantine.Hooks.Utilities
  ( useClipboard
  , useClipboard_
  , UseClipboard
  , UseClipboardResult
  , module Effect.Exception
  , useDocumentTitle
  , UseDocumentTitle
  , useDocumentVisibility
  , UseDocumentVisibility
  , DocumentVisibility(..)
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
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1)
import React.Basic.Hooks (type (/\), Hook, (/\), unsafeHook)
import Web.Event.Event (Event)

type UseClipboardResult =
  { copy   :: String -> Effect Unit
  , reset  :: Effect Unit
  , copied :: Boolean
  }

type UseClipboardResultImpl =
  { copy   :: EffectFn1 String Unit
  , reset  :: Effect Unit
  , copied :: Boolean
  , error  :: Nullable Error
  }

foreign import useClipboardImpl :: EffectFn1 { timeout :: Number } UseClipboardResultImpl
foreign import data UseClipboard :: Type -> Type

useClipboard :: { timeout :: Number } -> Hook UseClipboard (Either Error UseClipboardResult)
useClipboard options =
  let toNative result = case toMaybe result.error of
        Just e -> Left e
        Nothing -> Right
          { copy:   runEffectFn1 result.copy
          , copied: result.copied
          , reset:  result.reset
          }
   in unsafeHook (toNative <$> runEffectFn1 useClipboardImpl options)

useClipboard_ :: Hook UseClipboard (Either Error UseClipboardResult)
useClipboard_ = useClipboard { timeout: 2000.0 }

foreign import useDocumentTitleImpl :: EffectFn1 String Unit
foreign import data UseDocumentTitle :: Type -> Type

useDocumentTitle :: String -> Hook UseDocumentTitle Unit
useDocumentTitle t = unsafeHook (runEffectFn1 useDocumentTitleImpl t)

foreign import useDocumentVisibilityImpl :: Effect String
foreign import data UseDocumentVisibility :: Type -> Type

data DocumentVisibility = DocumentVisible | DocumentHidden

derive instance Eq DocumentVisibility

useDocumentVisibility :: Hook UseDocumentVisibility DocumentVisibility
useDocumentVisibility =
  let parseVisibility = case _ of
        "visible" -> DocumentVisible
        _         -> DocumentHidden
   in unsafeHook (parseVisibility <$> useDocumentVisibilityImpl)

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
