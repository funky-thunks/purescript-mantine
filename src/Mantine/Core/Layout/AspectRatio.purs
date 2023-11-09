module Mantine.Core.Layout.AspectRatio
  ( aspectRatio
  , AspectRatioProps
  ) where

import Mantine.Core.Prelude

aspectRatio :: (AspectRatioProps -> AspectRatioProps) -> JSX
aspectRatio = mkComponentWithDefault aspectRatioComponent defaultAspectRatioProps

foreign import aspectRatioComponent :: ReactComponent AspectRatioPropsImpl

type AspectRatioProps =
  MantineComponent
    ( children :: Array JSX
    , ratio    :: Number
    )

defaultAspectRatioProps :: AspectRatioProps
defaultAspectRatioProps = defaultMantineComponent { ratio: 1.0 }

type AspectRatioPropsImpl =
  MantineComponentImpl
    ( children :: Array JSX
    , ratio    :: Number
    )
