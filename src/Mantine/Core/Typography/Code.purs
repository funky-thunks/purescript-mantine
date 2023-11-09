module Mantine.Core.Typography.Code
  ( code
  , code_
  , CodeProps
  ) where

import Mantine.Core.Prelude

code :: (CodeProps -> CodeProps) -> JSX
code = mkTrivialComponent codeComponent

code_ :: Array JSX -> JSX
code_ children = code _ { children = children }

foreign import codeComponent :: ReactComponent CodePropsImpl

type CodeProps =
  MantineComponent
    ( block    :: Boolean
    , children :: Array JSX
    , color    :: Maybe MantineColor
    )

type CodePropsImpl =
  MantineComponentImpl
    ( block    :: Boolean
    , children :: Array JSX
    , color    :: Nullable MantineColorImpl
    )
