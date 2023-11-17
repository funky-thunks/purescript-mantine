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
    , position     :: Optional AffixPosition
    , withinPortal :: Boolean
    , zIndex       :: Optional ZIndex
    )

type AffixPosition =
  { top    :: Optional Dimension
  , bottom :: Optional Dimension
  , left   :: Optional Dimension
  , right  :: Optional Dimension
  }

defaultAffixProps :: AffixProps
defaultAffixProps = defaultMantineComponent { withinPortal: true }

type AffixPropsImpl =
  MantineComponentImpl
    ( children     :: Array JSX
    , position     :: OptionalImpl AffixPositionImpl
    , withinPortal :: Boolean
    , zIndex       :: OptionalImpl ZIndexImpl
    )

type AffixPositionImpl =
  { top    :: OptionalImpl DimensionImpl
  , bottom :: OptionalImpl DimensionImpl
  , left   :: OptionalImpl DimensionImpl
  , right  :: OptionalImpl DimensionImpl
  }
