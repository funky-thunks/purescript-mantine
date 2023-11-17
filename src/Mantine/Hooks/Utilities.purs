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

  , useNetwork
  , UseNetwork
  , UseNetworkResult
  , EffectiveType
  , NetworkType

  , useOS
  , useOS_
  , UseOS
  , UseOSOptions
  , OS(..)

  , usePageLeave
  , UsePageLeave

  , useTextSelection
  , UseTextSelection
  , Selection
  , getSelectedText
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

foreign import useNetworkImpl :: Effect UseNetworkResultImpl
foreign import data UseNetwork :: Type -> Type

data EffectiveType
  = EffectiveTypeSlow2G
  | EffectiveType2G
  | EffectiveType3G
  | EffectiveType4G

type EffectiveTypeImpl = String

instance FromFFI EffectiveTypeImpl EffectiveType where
  fromNative = case _ of
    "slow-2g" -> EffectiveTypeSlow2G
    "2g"      -> EffectiveType2G
    "3g"      -> EffectiveType3G
    "4g"      -> EffectiveType4G
    _         -> EffectiveType3G

data NetworkType
  = NetworkTypeBluetooth
  | NetworkTypeCellular
  | NetworkTypeEthernet
  | NetworkTypeWifi
  | NetworkTypeWimax
  | NetworkTypeNone
  | NetworkTypeOther
  | NetworkTypeUnknown

type NetworkTypeImpl = String

instance FromFFI NetworkTypeImpl NetworkType where
  fromNative = case _ of
    "bluetooth" -> NetworkTypeBluetooth
    "cellular"  -> NetworkTypeCellular
    "ethernet"  -> NetworkTypeEthernet
    "wifi"      -> NetworkTypeWifi
    "wimax"     -> NetworkTypeWimax
    "none"      -> NetworkTypeNone
    "other"     -> NetworkTypeOther
    _           -> NetworkTypeUnknown

type UseNetworkResult =
  { online         :: Boolean
  , downlink       :: Maybe Number
  , downlinkMax    :: Maybe Number
  , effectiveType  :: Maybe EffectiveType
  , rtt            :: Maybe Number
  , saveData       :: Boolean
  , type           :: Maybe NetworkType
  }

type UseNetworkResultImpl =
  { online         :: Boolean
  , downlink       :: Nullable Number
  , downlinkMax    :: Nullable Number
  , effectiveType  :: Nullable EffectiveTypeImpl
  , rtt            :: Nullable Number
  , saveData       :: Boolean
  , type           :: Nullable NetworkTypeImpl
  }

useNetwork :: Hook UseNetwork UseNetworkResult
useNetwork = mkHook0 useNetworkImpl

foreign import useHashImpl :: Effect { hash :: String, setHash :: EffectFn1 String Unit }
foreign import data UseHash :: Type -> Type

useHash :: Hook UseHash (String /\ (String -> Effect Unit))
useHash =
  let unpack { hash, setHash } = hash /\ setHash
   in unpack <$> mkHook0 useHashImpl

foreign import useHeadroomImpl :: EffectFn1 UseHeadroomOptionsImpl Boolean
foreign import data UseHeadroom :: Type -> Type

type UseHeadroomOptions =
  { fixedAt   :: Optional Number
  , onPin     :: Effect Unit
  , onFix     :: Effect Unit
  , onRelease :: Effect Unit
  }

type UseHeadroomOptionsImpl =
  { fixedAt   :: OptionalImpl Number
  , onPin     :: Effect Unit
  , onFix     :: Effect Unit
  , onRelease :: Effect Unit
  }

useHeadroom :: UseHeadroomOptions -> Hook UseHeadroom Boolean
useHeadroom = mkHook1 useHeadroomImpl

foreign import useOSImpl :: EffectFn1 (OptionalImpl UseOSOptions) OSImpl
foreign import data UseOS :: Type -> Type

data OS
  = OSUndetermined
  | OSMacOS
  | OSIOS
  | OSWindows
  | OSAndroid
  | OSLinux

type OSImpl = String

instance FromFFI OSImpl OS where
  fromNative = case _ of
    "undetermined" -> OSUndetermined
    "macos"        -> OSMacOS
    "ios"          -> OSIOS
    "windows"      -> OSWindows
    "android"      -> OSAndroid
    "linux"        -> OSLinux
    _              -> OSUndetermined

type UseOSOptions =
    { getValueInEffect :: Boolean
    }

useOS :: Maybe UseOSOptions -> Hook UseOS OS
useOS = useOS' <<< Optional

useOS' :: Optional UseOSOptions -> Hook UseOS OS
useOS' = mkHook1 useOSImpl

useOS_ :: Hook UseOS OS
useOS_ = useOS Nothing

foreign import usePageLeaveImpl :: EffectFn1 (Effect Unit) Unit
foreign import data UsePageLeave :: Type -> Type

usePageLeave :: Effect Unit -> Hook UsePageLeave Unit
usePageLeave = mkHook1 usePageLeaveImpl

foreign import useTextSelectionImpl :: Effect (Nullable Selection)
foreign import data UseTextSelection :: Type -> Type

useTextSelection :: Hook UseTextSelection (Maybe Selection)
useTextSelection = mkHook0 useTextSelectionImpl

foreign import data Selection :: Type

instance FromFFI Selection Selection where fromNative = identity

foreign import getSelectedTextImpl :: EffectFn1 Selection String

getSelectedText :: Selection -> Effect String
getSelectedText = runEffectFn1 getSelectedTextImpl
