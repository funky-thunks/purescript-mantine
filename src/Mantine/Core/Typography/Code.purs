module Mantine.Core.Typography.Code
  ( code
  , code_
  , Props_Code
  , Props_CodeImpl
  ) where

import Mantine.Core.Prelude

code
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Code
  => Union attrsImpl attrsImpl_ Props_CodeImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
code = element (unsafeCoerce codeComponent) <<< toNative

code_ :: Array JSX -> JSX
code_ children = code { children }

foreign import codeComponent :: ReactComponent (Record Props_CodeImpl)

type Props_Code =
  Props_Common
    ( block    :: Boolean
    , children :: Array JSX
    , color    :: MantineColor
    )

type Props_CodeImpl =
  Props_CommonImpl
    ( block    :: Boolean
    , children :: Array JSX
    , color    :: MantineColorImpl
    )
