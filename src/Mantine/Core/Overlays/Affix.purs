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
  MantineComponent
    ( children     :: Array JSX
    , position     :: Maybe AffixPosition
    , withinPortal :: Boolean
    , zIndex       :: Maybe ZIndex
    )

type AffixPosition =
  { top    :: Maybe Dimension
  , bottom :: Maybe Dimension
  , left   :: Maybe Dimension
  , right  :: Maybe Dimension
  }

defaultAffixProps :: AffixProps
defaultAffixProps = defaultMantineComponent { withinPortal: true }

type AffixPropsImpl =
  MantineComponentImpl
    ( children     :: Array JSX
    , position     :: Nullable AffixPositionImpl
    , withinPortal :: Boolean
    , zIndex       :: Nullable ZIndexImpl
    )

type AffixPositionImpl =
  { top    :: Nullable DimensionImpl
  , bottom :: Nullable DimensionImpl
  , left   :: Nullable DimensionImpl
  , right  :: Nullable DimensionImpl
  }
