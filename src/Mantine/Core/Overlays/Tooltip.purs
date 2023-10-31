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
    ( closeDelay :: Maybe Milliseconds
    , color      :: Maybe MantineColor
    , events     :: TooltipActivationEvents
    , inline     :: Boolean
    , label      :: Maybe JSX
    , multiline  :: Boolean
    , openDelay  :: Maybe Number
    , refProp    :: Maybe String
    )

defaultTooltipProps :: TooltipProps
defaultTooltipProps =
  defaultThemingProps
    { events:      { focus: false, hover: true, touch: false }
    , position:    HoverableFloatingPositionTop
    , withinPortal: true
    }

type TooltipActivationEvents = { hover :: Boolean, focus :: Boolean, touch :: Boolean }

type TooltipPropsImpl =
  HoverableComponentImpl
    ( closeDelay :: Nullable Milliseconds
    , color      :: Nullable String
    , events     :: TooltipActivationEvents
    , inline     :: Boolean
    , label      :: Nullable JSX
    , multiline  :: Boolean
    , openDelay  :: Nullable Milliseconds
    , refProp    :: Nullable String
    )

tooltipGroup :: (TooltipGroupProps -> TooltipGroupProps) -> JSX
tooltipGroup = mkTrivialComponent tooltipGroupComponent

foreign import tooltipGroupComponent :: ReactComponent TooltipGroupPropsImpl

type TooltipGroupProps = ThemingProps TooltipGroupRow

type TooltipGroupRow =
  ( children   :: Array JSX
  , closeDelay :: Milliseconds
  , openDelay  :: Milliseconds
  )

type TooltipGroupPropsImpl = ThemingPropsImpl TooltipGroupRow
