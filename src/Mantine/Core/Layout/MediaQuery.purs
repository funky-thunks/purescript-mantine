module Mantine.Core.Layout.MediaQuery
  ( mediaQuery
  , MediaQueryProps
  ) where

import Mantine.Core.Prelude

mediaQuery :: (MediaQueryProps -> MediaQueryProps) -> JSX
mediaQuery = mkComponentWithDefault mediaQueryComponent defaultThemingProps_

foreign import mediaQueryComponent :: ReactComponent MediaQueryPropsImpl

type MediaQueryProps =
  ThemingProps
    ( children    :: Array JSX
    , largerThan  :: Maybe MantineNumberSize
    , query       :: Maybe String
    , smallerThan :: Maybe MantineNumberSize
    )

type MediaQueryPropsImpl =
  ThemingPropsImpl
    ( children    :: Array JSX
    , largerThan  :: Nullable MantineNumberSizeImpl
    , query       :: Nullable String
    , smallerThan :: Nullable MantineNumberSizeImpl
    )
