module Mantine.Core.Overlays.Tooltip
  ( tooltip
  , tooltipFloating
  , Props_Tooltip
  , Props_TooltipImpl

  , TooltipActivationEvents

  , tooltipGroup
  , Props_TooltipGroup
  , Props_TooltipGroupImpl
  , TooltipGroupRow
  ) where

import Mantine.Core.Overlays.Hovering (HoverableComponent, HoverableComponentImpl)
import Mantine.Core.Prelude

tooltip
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Tooltip
  => Union attrsImpl attrsImpl_ Props_TooltipImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
tooltip = element (unsafeCoerce tooltipComponent) <<< toNative

tooltipFloating
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Tooltip
  => Union attrsImpl attrsImpl_ Props_TooltipImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
tooltipFloating = element (unsafeCoerce tooltipFloatingComponent) <<< toNative

foreign import tooltipComponent         :: ReactComponent (Record Props_TooltipImpl)
foreign import tooltipFloatingComponent :: ReactComponent (Record Props_TooltipImpl)

-- Not supported properties
--   { portalProps          :: Omit<PortalProps, "children" | "withinPortal">
--   , positionDependencies :: any[]
--   }

type Props_Tooltip =
  HoverableComponent
    ( closeDelay :: Milliseconds
    , color      :: MantineColor
    , events     :: TooltipActivationEvents
    , inline     :: Boolean
    , label      :: JSX
    , multiline  :: Boolean
    , onChange   :: ValueHandler Boolean
    , openDelay  :: Milliseconds
    , opened     :: Boolean
    , refProp    :: String
    )

type TooltipActivationEvents = { hover :: Boolean, focus :: Boolean, touch :: Boolean }

type Props_TooltipImpl =
  HoverableComponentImpl
    ( closeDelay :: MillisecondsImpl
    , color      :: MantineColorImpl
    , events     :: TooltipActivationEvents
    , inline     :: Boolean
    , label      :: JSX
    , multiline  :: Boolean
    , onChange   :: ValueHandlerImpl Boolean
    , openDelay  :: MillisecondsImpl
    , opened     :: Boolean
    , refProp    :: String
    )

tooltipGroup
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_TooltipGroup
  => Union attrsImpl attrsImpl_ Props_TooltipGroupImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
tooltipGroup = element (unsafeCoerce tooltipGroupComponent) <<< toNative

foreign import tooltipGroupComponent :: ReactComponent (Record Props_TooltipGroupImpl)

type Props_TooltipGroup     = Props_Common     TooltipGroupRow
type Props_TooltipGroupImpl = Props_CommonImpl TooltipGroupRow

type TooltipGroupRow =
  ( children   :: Array JSX
  , closeDelay :: Milliseconds
  , openDelay  :: Milliseconds
  )
