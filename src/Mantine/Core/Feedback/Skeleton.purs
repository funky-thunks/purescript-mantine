module Mantine.Core.Feedback.Skeleton
  ( skeleton
  , skeleton_
  , SkeletonProps
  ) where

import Mantine.Core.Prelude

skeleton :: (SkeletonProps -> SkeletonProps) -> JSX
skeleton = mkComponentWithDefault skeletonComponent defaultSkeletonProps

skeleton_ :: JSX
skeleton_ = skeleton identity

foreign import skeletonComponent :: ReactComponent SkeletonPropsImpl

type SkeletonProps =
  MantineComponent
    ( animate :: Boolean
    , circle  :: Boolean
    , height  :: Dimension
    , radius  :: Optional MantineNumberSize
    , visible :: Boolean
    , width   :: Optional Dimension
    )

defaultSkeletonProps :: SkeletonProps
defaultSkeletonProps =
  defaultMantineComponent
    { animate: true
    , height:  Dimension "auto"
    , visible: true
    }

type SkeletonPropsImpl =
  MantineComponentImpl
    ( animate :: Boolean
    , circle  :: Boolean
    , height  :: DimensionImpl
    , radius  :: OptionalImpl MantineNumberSizeImpl
    , visible :: Boolean
    , width   :: OptionalImpl DimensionImpl
    )
