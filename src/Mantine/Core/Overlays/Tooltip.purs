module Mantine.Core.Overlays.Tooltip
  ( tooltip
  , tooltipFloating
  , TooltipProps

  , TooltipActivationEvents

  , tooltipGroup
  , TooltipGroupProps
  , TooltipGroupRow
  ) where

import Mantine.Core.Overlays.Hovering (HoverableComponent, HoverableComponentImpl, HoverableFloatingPosition(..))
import Mantine.Core.Prelude

tooltip :: (TooltipProps -> TooltipProps) -> JSX
tooltip = mkComponentWithDefault tooltipComponent defaultTooltipProps

tooltipFloating :: (TooltipProps -> TooltipProps) -> JSX
tooltipFloating = mkComponentWithDefault tooltipFloatingComponent defaultTooltipProps

foreign import tooltipComponent         :: ReactComponent TooltipPropsImpl
foreign import tooltipFloatingComponent :: ReactComponent TooltipPropsImpl

-- Not supported properties
--   { portalProps          :: Omit<PortalProps, "children" | "withinPortal">
--   , positionDependencies :: any[]
--   }

type TooltipProps =
  HoverableComponent
    ( closeDelay :: Optional Milliseconds
    , color      :: Optional MantineColor
    , events     :: TooltipActivationEvents
    , inline     :: Boolean
    , label      :: Optional JSX
    , multiline  :: Boolean
    , openDelay  :: Optional Milliseconds
    , refProp    :: Optional String
    )

defaultTooltipProps :: TooltipProps
defaultTooltipProps =
  defaultMantineComponent
    { events:      { focus: false, hover: true, touch: false }
    , position:    HoverableFloatingPositionTop
    , withinPortal: true
    }

type TooltipActivationEvents = { hover :: Boolean, focus :: Boolean, touch :: Boolean }

type TooltipPropsImpl =
  HoverableComponentImpl
    ( closeDelay :: OptionalImpl MillisecondsImpl
    , color      :: OptionalImpl MantineColorImpl
    , events     :: TooltipActivationEvents
    , inline     :: Boolean
    , label      :: OptionalImpl JSX
    , multiline  :: Boolean
    , openDelay  :: OptionalImpl MillisecondsImpl
    , refProp    :: OptionalImpl String
    )

tooltipGroup :: (TooltipGroupProps -> TooltipGroupProps) -> JSX
tooltipGroup = mkTrivialComponent tooltipGroupComponent

foreign import tooltipGroupComponent :: ReactComponent TooltipGroupPropsImpl

type TooltipGroupProps     = MantineComponent     TooltipGroupRow
type TooltipGroupPropsImpl = MantineComponentImpl TooltipGroupRow

type TooltipGroupRow =
  ( children   :: Array JSX
  , closeDelay :: Milliseconds
  , openDelay  :: Milliseconds
  )
