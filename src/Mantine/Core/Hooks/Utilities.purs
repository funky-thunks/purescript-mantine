module Mantine.Core.Hooks.Utilities
  ( useDocumentTitle
  , UseDocumentTitle
  , useFavicon
  , UseFavicon
  ) where

import Prelude
import Effect (Effect)
import React.Basic.Hooks (Hook, unsafeHook)

foreign import useDocumentTitleImpl :: String -> Effect Unit
foreign import data UseDocumentTitle :: Type -> Type

useDocumentTitle :: String -> Hook UseDocumentTitle Unit
useDocumentTitle t = unsafeHook (useDocumentTitleImpl t)

foreign import useFaviconImpl :: String -> Effect Unit
foreign import data UseFavicon :: Type -> Type

useFavicon :: String -> Hook UseFavicon Unit
useFavicon t = unsafeHook (useFaviconImpl t)
