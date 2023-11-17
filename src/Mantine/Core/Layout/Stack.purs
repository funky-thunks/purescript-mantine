module Mantine.Core.Layout.Stack
  ( stack
  , stack_
  , Props_Stack
  , Props_StackImpl
  ) where

import Mantine.Core.Prelude

stack
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Stack
  => Union attrsImpl attrsImpl_ Props_StackImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
stack = element (unsafeCoerce stackComponent) <<< toNative

stack_ :: Array JSX -> JSX
stack_ children = stack { children }

foreign import stackComponent :: ReactComponent (Record Props_StackImpl)

type Props_Stack =
  Props_Common
    ( align    :: AlignItems
    , children :: Array JSX
    , gap      :: MantineSpacing
    , justify  :: JustifyContent
    )

type Props_StackImpl =
  Props_CommonImpl
    ( align    :: AlignItemsImpl
    , children :: Array JSX
    , gap      :: MantineSpacingImpl
    , justify  :: JustifyContentImpl
    )
