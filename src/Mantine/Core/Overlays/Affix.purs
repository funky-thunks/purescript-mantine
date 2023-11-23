module Mantine.Core.Overlays.Affix
  ( affix
  , affix_
  , Props_Affix
  , Props_AffixImpl
  , AffixPosition
  , AffixPositionImpl
  ) where

import Mantine.Core.Prelude

affix
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Affix
  => Union attrsImpl attrsImpl_ Props_AffixImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
affix = element (unsafeCoerce affixComponent) <<< toNative

affix_ :: Array JSX -> JSX
affix_ children = affix { children }

foreign import affixComponent :: ReactComponent (Record Props_AffixImpl)

-- Not supported properties
--   { portalProps :: Omit<PortalProps, "children">
--   }

type Props_Affix =
  Props_Common
    ( children     :: Array JSX
    , position     :: AffixPosition
    , withinPortal :: Boolean
    , zIndex       :: ZIndex
    )

type AffixPosition =
  { top    :: Optional Dimension
  , bottom :: Optional Dimension
  , left   :: Optional Dimension
  , right  :: Optional Dimension
  }

type Props_AffixImpl =
  Props_CommonImpl
    ( children     :: Array JSX
    , position     :: AffixPositionImpl
    , withinPortal :: Boolean
    , zIndex       :: ZIndexImpl
    )

type AffixPositionImpl =
  { top    :: OptionalImpl DimensionImpl
  , bottom :: OptionalImpl DimensionImpl
  , left   :: OptionalImpl DimensionImpl
  , right  :: OptionalImpl DimensionImpl
  }
