module Mantine.Core.Feedback.Progress
  ( progress
  , ProgressProps
  , ProgressSection
  ) where

import Mantine.Core.Prelude

progress :: (ProgressProps -> ProgressProps) -> JSX
progress = mkTrivialComponent progressComponent

foreign import progressComponent :: ReactComponent ProgressPropsImpl

type ProgressProps =
  ThemingProps
    ( animate  :: Boolean
    , color    :: Maybe MantineColor
    , label    :: Maybe String
    , radius   :: Maybe MantineNumberSize
    , sections :: Array ProgressSection
    , size     :: Maybe MantineNumberSize
    , striped  :: Boolean
    , value    :: Number
    )

type ProgressSection =
  { color        :: Maybe MantineColor
  , label        :: Maybe String
  , onMouseEnter :: EventHandler
  , onMouseLeave :: EventHandler
  , tooltip      :: Maybe JSX
  , value        :: Number
  }

type ProgressPropsImpl =
  ThemingPropsImpl
    ( animate  :: Boolean
    , color    :: Nullable String
    , label    :: Nullable String
    , radius   :: Nullable MantineNumberSizeImpl
    , sections :: Array ProgressSectionImpl
    , size     :: Nullable MantineNumberSizeImpl
    , striped  :: Boolean
    , value    :: Number
    )

type ProgressSectionImpl =
  { color        :: Nullable String
  , label        :: Nullable String
  , onMouseEnter :: EventHandler
  , onMouseLeave :: EventHandler
  , tooltip      :: Nullable JSX
  , value        :: Number
  }
