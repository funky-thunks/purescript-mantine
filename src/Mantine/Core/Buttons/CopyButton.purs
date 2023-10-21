module Mantine.Core.Buttons.CopyButton
  ( copyButton
  , CopyButtonProps
  ) where

import Mantine.Core.Prelude
import React.Basic (element)

copyButton :: CopyButtonProps -> JSX
copyButton = element copyButtonComponent <<< copyButtonPropsToImpl

foreign import copyButtonComponent :: ReactComponent CopyButtonPropsImpl

type CopyButtonProps =
  { children :: { copied :: Boolean, copy :: Effect Unit } -> JSX
  , value    :: String
  , timeout  :: Maybe Number
  }

type CopyButtonPropsImpl =
  { children :: { copied :: Boolean, copy :: Effect Unit } -> JSX
  , timeout  :: Nullable Number
  , value    :: String
  }

copyButtonPropsToImpl :: CopyButtonProps -> CopyButtonPropsImpl
copyButtonPropsToImpl props@{ timeout } = props { timeout = toNative timeout }
