module Mantine.Core.Layout.Flex
  ( flex
  , flex_
  , Props_Flex
  , Props_FlexImpl
  ) where

import Mantine.Core.Prelude

flex
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Flex
  => Union attrsImpl attrsImpl_ Props_FlexImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
flex = element (unsafeCoerce flexComponent) <<< toNative

flex_ :: Array JSX -> JSX
flex_ children = flex { children }

foreign import flexComponent :: ReactComponent (Record Props_FlexImpl)

type Props_Flex =
  Props_Common
    ( align     :: AlignItems
    , children  :: Array JSX
    , columnGap :: MantineSize
    , direction :: FlexDirection
    , gap       :: MantineSize
    , justify   :: JustifyContent
    , rowGap    :: MantineSize
    , wrap      :: FlexWrap
    )

type Props_FlexImpl =
  Props_CommonImpl
    ( align     :: AlignItemsImpl
    , children  :: Array JSX
    , columnGap :: MantineSizeImpl
    , direction :: FlexDirectionImpl
    , gap       :: MantineSizeImpl
    , justify   :: JustifyContentImpl
    , rowGap    :: MantineSizeImpl
    , wrap      :: FlexWrapImpl
    )
