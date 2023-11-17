module Mantine.Core.Layout.Group
  ( group
  , group_
  , Props_Group
  , Props_GroupImpl
  ) where

import Mantine.Core.Prelude

group
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Group
  => Union attrsImpl attrsImpl_ Props_GroupImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
group = element (unsafeCoerce groupComponent) <<< toNative

group_ :: Array JSX -> JSX
group_ children = group { children }

foreign import groupComponent :: ReactComponent (Record Props_GroupImpl)

type Props_Group =
  Props_Common
    ( align               :: AlignItems
    , children            :: Array JSX
    , gap                 :: MantineSpacing
    , grow                :: Boolean
    , justify             :: JustifyContent
    , preventGrowOverflow :: Boolean
    , wrap                :: FlexWrap
    )

type Props_GroupImpl =
  Props_CommonImpl
    ( align               :: AlignItemsImpl
    , children            :: Array JSX
    , gap                 :: MantineSpacingImpl
    , grow                :: Boolean
    , justify             :: JustifyContentImpl
    , preventGrowOverflow :: Boolean
    , wrap                :: FlexWrapImpl
    )
