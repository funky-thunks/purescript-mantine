module Mantine.Hooks.Utilities
  ( useClipboard
  , useClipboard_
  , UseClipboard
  , UseClipboardResult
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

import Mantine.Hooks.Prelude

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
  let unpackError result = case result.error of
        Just e -> Left e
        Nothing -> Right
          { copy:   result.copy
          , copied: result.copied
          , reset:  result.reset
          }
   in unpackError <$> mkHook1 useClipboardImpl options

useClipboard_ :: Hook UseClipboard (Either Error UseClipboardResult)
useClipboard_ = useClipboard { timeout: 2000.0 }

foreign import useDocumentTitleImpl :: EffectFn1 String Unit
foreign import data UseDocumentTitle :: Type -> Type

useDocumentTitle :: String -> Hook UseDocumentTitle Unit
useDocumentTitle = mkHook1 useDocumentTitleImpl

foreign import useDocumentVisibilityImpl :: Effect String
foreign import data UseDocumentVisibility :: Type -> Type

data DocumentVisibility = DocumentVisible | DocumentHidden

type DocumentVisibilityImpl = String

instance FromFFI DocumentVisibilityImpl DocumentVisibility where
  fromNative = case _ of
    "visible" -> DocumentVisible
    _         -> DocumentHidden

derive instance Eq DocumentVisibility

useDocumentVisibility :: Hook UseDocumentVisibility DocumentVisibility
useDocumentVisibility = mkHook0 useDocumentVisibilityImpl

foreign import useFaviconImpl :: EffectFn1 String Unit
foreign import data UseFavicon :: Type -> Type

useFavicon :: String -> Hook UseFavicon Unit
useFavicon = mkHook1 useFaviconImpl

foreign import useHashImpl :: Effect { hash :: String, setHash :: EffectFn1 String Unit }
foreign import data UseHash :: Type -> Type

useHash :: Hook UseHash (String /\ (String -> Effect Unit))
useHash =
  let unpack { hash, setHash } = hash /\ setHash
   in unpack <$> mkHook0 useHashImpl

foreign import usePageLeaveImpl :: EffectFn1 (Effect Unit) Unit
foreign import data UsePageLeave :: Type -> Type

usePageLeave :: Effect Unit -> Hook UsePageLeave Unit
usePageLeave = mkHook1 usePageLeaveImpl

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
useWindowEvent = mkHook1 useWindowEventImpl

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
  let unpack { current, moveTo } = current /\ moveTo
   in unpack <$> mkHook0 useWindowScrollImpl
