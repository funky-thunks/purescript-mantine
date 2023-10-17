module Mantine.Core.Feedback.Skeleton
  ( skeleton
  , skeleton_
  , SkeletonProps

  , module Mantine.Core.Common
  ) where

import Prelude
import Data.Default (defaultValue)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Mantine.Core.Common (Dimension, MantineNumberSize, MantineSize(..))
import Mantine.Core.Common as MC
import Mantine.FFI (toNative)
import React.Basic (ReactComponent, element)
import React.Basic.Hooks (JSX)
import Record (union)

skeleton :: (SkeletonProps -> SkeletonProps) -> JSX
skeleton setProps = element skeletonComponent (toNative (setProps defaultSkeletonProps))

skeleton_ :: JSX
skeleton_ = skeleton identity

foreign import skeletonComponent :: ReactComponent SkeletonPropsImpl

type SkeletonProps =
  MC.ThemingProps
    ( animate :: Boolean
    , circle  :: Boolean
    , height  :: Dimension
    , radius  :: Maybe MantineNumberSize
    , visible :: Boolean
    , width   :: Maybe Dimension
    )

defaultSkeletonProps :: SkeletonProps
defaultSkeletonProps =
  MC.defaultThemingProps
    { animate: true
    , height:  pure "auto"
    , visible: true
    } `union` defaultValue

type SkeletonPropsImpl =
  MC.ThemingPropsImpl
    ( animate :: Boolean
    , circle  :: Boolean
    , height  :: MC.DimensionImpl
    , radius  :: Nullable MC.MantineNumberSizeImpl
    , visible :: Boolean
    , width   :: Nullable MC.DimensionImpl
    )
