module Mantine.Core.Buttons.CopyButton
  ( copyButton
  , Props_CopyButton
  , Props_CopyButtonImpl
  ) where

import Mantine.Core.Prelude

copyButton
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_CopyButton
  => Union attrsImpl attrsImpl_ Props_CopyButtonImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
copyButton = element (unsafeCoerce copyButtonComponent) <<< toNative

foreign import copyButtonComponent :: ReactComponent (Record Props_CopyButtonImpl)

type Props_CopyButton =
  Props_Common
    ( children :: { copied :: Boolean, copy :: Effect Unit } -> JSX
    , timeout  :: Number
    , value    :: String
    )

type Props_CopyButtonImpl =
  Props_CommonImpl
    ( children :: { copied :: Boolean, copy :: Effect Unit } -> JSX
    , timeout  :: Number
    , value    :: String
    )
