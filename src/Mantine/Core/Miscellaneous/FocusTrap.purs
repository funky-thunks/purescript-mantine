module Mantine.Core.Miscellaneous.FocusTrap
  ( focusTrap
  , focusTrap_
  , FocusTrapProps
  ) where

import Mantine.Core.Prelude

focusTrap :: (FocusTrapProps -> FocusTrapProps) -> JSX
focusTrap = mkComponent focusTrapComponent identity defaultValue

focusTrap_ :: Array JSX -> JSX
focusTrap_ children = focusTrap _ { children = children }

foreign import focusTrapComponent :: ReactComponent FocusTrapProps

type FocusTrapProps =
  { active   :: Boolean
  , children :: Array JSX
  , refProp  :: String
  }
