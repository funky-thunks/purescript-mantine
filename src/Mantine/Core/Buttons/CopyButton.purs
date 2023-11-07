module Mantine.Core.Buttons.CopyButton
  ( copyButton
  , CopyButtonProps
  , CopyButtonMandatoryProps
  , CopyButtonMandatoryPropsRow
  ) where

import Mantine.Core.Prelude

copyButton :: CopyButtonMandatoryProps -> (CopyButtonProps -> CopyButtonProps) -> JSX
copyButton = mkComponent copyButtonComponent copyButtonPropsToImpl <<< defaultThemingProps

foreign import copyButtonComponent :: ReactComponent CopyButtonPropsImpl

type CopyButtonProps =
  ThemingProps
    ( timeout  :: Maybe Number
    | CopyButtonMandatoryPropsRow
    )

type CopyButtonMandatoryProps = Record CopyButtonMandatoryPropsRow

type CopyButtonMandatoryPropsRow =
  ( children :: { copied :: Boolean, copy :: Effect Unit } -> JSX
  , value    :: String
  )

type CopyButtonPropsImpl =
  ThemingPropsImpl
    ( children :: { copied :: Boolean, copy :: Effect Unit } -> JSX
    , timeout  :: Nullable Number
    , value    :: String
    )

copyButtonPropsToImpl :: CopyButtonProps -> CopyButtonPropsImpl
copyButtonPropsToImpl props =
  let rest = toNative
         <<< delete (Proxy :: Proxy "children")
   in { children: props.children } `union` rest props
