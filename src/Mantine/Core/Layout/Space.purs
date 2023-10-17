module Mantine.Core.Layout.Space
  ( space
  , SpaceProps
  ) where

import Mantine.Core.Prelude

space :: (SpaceProps -> SpaceProps) -> JSX
space = mkComponentWithDefault spaceComponent defaultThemingProps_

foreign import spaceComponent :: ReactComponent SpacePropsImpl

type SpaceProps =
  ThemingProps
    ( h :: Maybe MantineNumberSize
    , w :: Maybe MantineNumberSize
    )

type SpacePropsImpl =
  ThemingPropsImpl
    ( h :: Nullable MantineNumberSizeImpl
    , w :: Nullable MantineNumberSizeImpl
    )
