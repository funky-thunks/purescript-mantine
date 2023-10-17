module Mantine.Core.Feedback.Progress
  ( progress
  , ProgressProps
  , ProgressSection

  , module Mantine.Core.Common
  ) where

import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Mantine.Core.Common (MantineColor(..), MantineNumberSize, MantineSize(..))
import Mantine.Core.Common as MC
import Mantine.FFI (toNative)
import React.Basic (ReactComponent, element)
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (JSX)

progress :: (ProgressProps -> ProgressProps) -> JSX
progress setProps = element progressComponent (toNative (setProps MC.defaultThemingProps_))

foreign import progressComponent :: ReactComponent ProgressPropsImpl

type ProgressProps =
  MC.ThemingProps
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
  MC.ThemingPropsImpl
    ( animate  :: Boolean
    , color    :: Nullable String
    , label    :: Nullable String
    , radius   :: Nullable MC.MantineNumberSizeImpl
    , sections :: Array ProgressSectionImpl
    , size     :: Nullable MC.MantineNumberSizeImpl
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
