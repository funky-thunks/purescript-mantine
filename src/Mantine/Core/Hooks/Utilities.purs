module Mantine.Core.Hooks.Utilities
  ( useDocumentTitle
  , UseDocumentTitle
  , useFavicon
  , UseFavicon
  , useHash
  , UseHash
  , usePageLeave
  , UsePageLeave
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import React.Basic.Hooks (type (/\), Hook, (/\), unsafeHook)

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
