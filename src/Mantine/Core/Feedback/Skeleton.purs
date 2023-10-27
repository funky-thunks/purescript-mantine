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
  ThemingProps
    ( animate :: Boolean
    , circle  :: Boolean
    , height  :: Dimension
    , radius  :: Maybe MantineNumberSize
    , visible :: Boolean
    , width   :: Maybe Dimension
    )

defaultSkeletonProps :: SkeletonProps
defaultSkeletonProps =
  defaultThemingProps
    { animate: true
    , height:  Dimension "auto"
    , visible: true
    }

type SkeletonPropsImpl =
  ThemingPropsImpl
    ( animate :: Boolean
    , circle  :: Boolean
    , height  :: DimensionImpl
    , radius  :: Nullable MantineNumberSizeImpl
    , visible :: Boolean
    , width   :: Nullable DimensionImpl
    )
