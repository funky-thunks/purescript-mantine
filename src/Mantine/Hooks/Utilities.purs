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

  , useHeadroom
  , UseHeadroom
  , UseHeadroomOptions

  , useIdle
  , UseIdle

  , usePageLeave
  , UsePageLeave
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

foreign import useIdleImpl :: EffectFn1 Number Boolean

foreign import data UseIdle :: Type -> Type

useIdle :: Number -> Hook UseIdle Boolean
useIdle = mkHook1 useIdleImpl

foreign import useHashImpl :: Effect { hash :: String, setHash :: EffectFn1 String Unit }
foreign import data UseHash :: Type -> Type

useHash :: Hook UseHash (String /\ (String -> Effect Unit))
useHash =
  let unpack { hash, setHash } = hash /\ setHash
   in unpack <$> mkHook0 useHashImpl

foreign import useHeadroomImpl :: EffectFn1 UseHeadroomOptionsImpl Boolean
foreign import data UseHeadroom :: Type -> Type

type UseHeadroomOptions =
  { fixedAt   :: Maybe Number
  , onPin     :: Effect Unit
  , onFix     :: Effect Unit
  , onRelease :: Effect Unit
  }

type UseHeadroomOptionsImpl =
  { fixedAt   :: Nullable Number
  , onPin     :: Effect Unit
  , onFix     :: Effect Unit
  , onRelease :: Effect Unit
  }

useHeadroom :: UseHeadroomOptions -> Hook UseHeadroom Boolean
useHeadroom = mkHook1 useHeadroomImpl

foreign import usePageLeaveImpl :: EffectFn1 (Effect Unit) Unit
foreign import data UsePageLeave :: Type -> Type

usePageLeave :: Effect Unit -> Hook UsePageLeave Unit
usePageLeave = mkHook1 usePageLeaveImpl
