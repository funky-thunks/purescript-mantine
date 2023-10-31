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

-- Not supported properties
--   { portalProps :: Omit<PortalProps, "children">
--   }

type AffixProps =
  ThemingProps
    ( children     :: Array JSX
    , position     :: Maybe AffixPosition
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
defaultAffixProps = defaultThemingProps { withinPortal: true }

type AffixPropsImpl =
  ThemingPropsImpl
    ( children     :: Array JSX
    , position     :: Nullable AffixPositionImpl
    , withinPortal :: Boolean
    , zIndex       :: Nullable Number
    )

type AffixPositionImpl =
  { top    :: Nullable DimensionImpl
  , bottom :: Nullable DimensionImpl
  , left   :: Nullable DimensionImpl
  , right  :: Nullable DimensionImpl
  }
