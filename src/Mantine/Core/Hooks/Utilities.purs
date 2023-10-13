module Mantine.Core.Hooks.Utilities
  ( useDocumentTitle
  , UseDocumentTitle
  ) where

import Prelude
import Effect (Effect)
import React.Basic.Hooks (Hook, unsafeHook)

foreign import useDocumentTitleImpl :: String -> Effect Unit

foreign import data UseDocumentTitle :: Type -> Type

useDocumentTitle :: String -> Hook UseDocumentTitle Unit
useDocumentTitle t = unsafeHook (useDocumentTitleImpl t)
