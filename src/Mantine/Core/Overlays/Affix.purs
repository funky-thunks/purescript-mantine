module Mantine.Core.Overlays.Affix
  ( affix
  , affix_
  , AffixProps
  , AffixPosition
  ) where

import Mantine.Core.Prelude

affix :: (AffixProps -> AffixProps) -> JSX
affix = mkComponentWithDefault affixComponent defaultAffixProps

affix_ :: Array JSX -> JSX
affix_ children = affix _ { children = children }

foreign import affixComponent :: ReactComponent AffixPropsImpl

type AffixProps =
  ThemingProps
    ( children     :: Array JSX
    , position     :: AffixPosition
    , withinPortal :: Boolean
    , zIndex       :: Maybe Number
    )

type AffixPosition =
  { top    :: Maybe Dimension
  , bottom :: Maybe Dimension
  , left   :: Maybe Dimension
  , right  :: Maybe Dimension
  }

defaultAffixProps :: AffixProps
defaultAffixProps =
  defaultThemingProps
    { withinPortal: true
    } `union` defaultValue

type AffixPropsImpl =
  ThemingPropsImpl
    ( children     :: Array JSX
    , position     :: AffixPositionImpl
    , withinPortal :: Boolean
    , zIndex       :: Nullable Number
    )

type AffixPositionImpl =
  { top    :: Nullable DimensionImpl
  , bottom :: Nullable DimensionImpl
  , left   :: Nullable DimensionImpl
  , right  :: Nullable DimensionImpl
  }
