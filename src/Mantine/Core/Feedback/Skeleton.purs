module Mantine.Core.Feedback.Skeleton
  ( skeleton
  , skeleton_
  , Props_Skeleton
  , Props_SkeletonImpl
  ) where

import Mantine.Core.Prelude

skeleton
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Skeleton
  => Union attrsImpl attrsImpl_ Props_SkeletonImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
skeleton = element (unsafeCoerce skeletonComponent) <<< toNative

skeleton_ :: JSX
skeleton_ = skeleton {}

foreign import skeletonComponent :: ReactComponent (Record Props_SkeletonImpl)

type Props_Skeleton =
  Props_Common
    ( animate :: Boolean
    , circle  :: Boolean
    , height  :: Dimension
    , radius  :: MantineNumberSize
    , visible :: Boolean
    , width   :: Dimension
    )

type Props_SkeletonImpl =
  Props_CommonImpl
    ( animate :: Boolean
    , circle  :: Boolean
    , height  :: DimensionImpl
    , radius  :: MantineNumberSizeImpl
    , visible :: Boolean
    , width   :: DimensionImpl
    )
