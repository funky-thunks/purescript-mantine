module Mantine.Core.Overlays.Tooltip
  ( tooltip
  , TooltipProps

  , tooltipFloating
  , TooltipFloatingProps

  , TooltipActivationEvents
  , TooltipPosition(..)
  , TooltipPropsBaseRow
  , TooltipPropsRow

  , tooltipGroup
  , TooltipGroupProps
  , TooltipGroupRow
  ) where

import Mantine.Core.Prelude
import React.Basic.DOM as DOM

tooltip :: (TooltipProps -> TooltipProps) -> JSX
tooltip = mkComponent tooltipComponent tooltipToImpl defaultTooltipProps

tooltipFloating :: (TooltipFloatingProps -> TooltipFloatingProps) -> JSX
tooltipFloating = mkComponent tooltipFloatingComponent tooltipFloatingToImpl defaultTooltipFloatingProps

foreign import tooltipComponent         :: ReactComponent TooltipPropsImpl
foreign import tooltipFloatingComponent :: ReactComponent TooltipFloatingPropsImpl

type TooltipProps = ThemingProps (TooltipPropsBaseRow + TooltipPropsRow)

type TooltipPropsBaseRow r =
  ( children     :: JSX
  , color        :: Maybe MantineColor
  , disabled     :: Boolean
  , label        :: Maybe JSX
  , multiline    :: Boolean
  , offset       :: Pixels
  , position     :: TooltipPosition
  , radius       :: Maybe MantineNumberSize
  , width        :: Maybe Dimension
  , withinPortal :: Boolean
  , zIndex       :: Maybe Number
  | r
  )

type TooltipPropsRow =
  ( arrowOffset      :: Pixels
  , arrowRadius      :: Pixels
  , arrowSize        :: Pixels
  , closeDelay       :: Maybe Milliseconds
  , events           :: TooltipActivationEvents
  , inline           :: Boolean
  , onPositionChange :: ValueHandler TooltipPosition
  , openDelay        :: Milliseconds
  , opened           :: Maybe Boolean
  , transition       :: MantineTransition
  , withArrow        :: Boolean
  )

data TooltipPosition
  = TooltipPositionTop
  | TooltipPositionRight
  | TooltipPositionBottom
  | TooltipPositionLeft
  | TooltipPositionTopStart
  | TooltipPositionRightStart
  | TooltipPositionBottomStart
  | TooltipPositionLeftStart
  | TooltipPositionTopEnd
  | TooltipPositionRightEnd
  | TooltipPositionBottomEnd
  | TooltipPositionLeftEnd

instance ToFFI TooltipPosition String where
  toNative = case _ of
    TooltipPositionTop         -> "top"
    TooltipPositionRight       -> "right"
    TooltipPositionBottom      -> "bottom"
    TooltipPositionLeft        -> "left"
    TooltipPositionTopStart    -> "top-start"
    TooltipPositionRightStart  -> "right-start"
    TooltipPositionBottomStart -> "bottom-start"
    TooltipPositionLeftStart   -> "left-start"
    TooltipPositionTopEnd      -> "top-end"
    TooltipPositionRightEnd    -> "right-end"
    TooltipPositionBottomEnd   -> "bottom-end"
    TooltipPositionLeftEnd     -> "left-end"

instance FromFFI String TooltipPosition where
  fromNative = case _ of
    "top"          -> TooltipPositionTop
    "right"        -> TooltipPositionRight
    "bottom"       -> TooltipPositionBottom
    "left"         -> TooltipPositionLeft
    "top-start"    -> TooltipPositionTopStart
    "right-start"  -> TooltipPositionRightStart
    "bottom-start" -> TooltipPositionBottomStart
    "left-start"   -> TooltipPositionLeftStart
    "top-end"      -> TooltipPositionTopEnd
    "right-end"    -> TooltipPositionRightEnd
    "bottom-end"   -> TooltipPositionBottomEnd
    "left-end"     -> TooltipPositionLeftEnd
    _              -> TooltipPositionTop

type TooltipActivationEvents = { hover :: Boolean, focus :: Boolean, touch :: Boolean }

type TooltipFloatingProps =
  ThemingProps (TooltipPropsBaseRow ())

defaultTooltipFloatingProps :: TooltipFloatingProps
defaultTooltipFloatingProps =
  defaultThemingProps
    { children:     mempty :: JSX
    , offset:       10.0
    , position:     TooltipPositionRight
    , withinPortal: true
    }

defaultTooltipProps :: TooltipProps
defaultTooltipProps =
  defaultThemingProps
    { arrowOffset: 5.0
    , arrowRadius: 0.0
    , arrowSize:   4.0
    , children:    mempty :: JSX
    , events:      { focus: false, hover: true, touch: false }
    , offset:      5.0
    , openDelay:   0.0
    , position:    TooltipPositionTop
    , transition:  TransitionFade
    , width:       pure (Dimension "auto")
    }

type TooltipPropsImpl = ThemingPropsImpl (TooltipPropsBaseImplRow + TooltipPropsImplRow)

type TooltipFloatingPropsImpl = ThemingPropsImpl (TooltipPropsBaseImplRow ())

type TooltipPropsBaseImplRow r =
  ( children     :: JSX
  , color        :: Nullable String
  , disabled     :: Boolean
  , label        :: Nullable JSX
  , multiline    :: Boolean
  , offset       :: Pixels
  , position     :: String
  , radius       :: Nullable MantineNumberSizeImpl
  , width        :: Nullable DimensionImpl
  , withinPortal :: Boolean
  , zIndex       :: Nullable Number
  | r
  )

type TooltipPropsImplRow =
  ( arrowOffset      :: Pixels
  , arrowRadius      :: Pixels
  , arrowSize        :: Pixels
  , closeDelay       :: Nullable Milliseconds
  , events           :: TooltipActivationEvents
  , inline           :: Boolean
  , onPositionChange :: EffectFn1 String Unit
  , openDelay        :: Milliseconds
  , opened           :: Nullable Boolean
  , transition       :: String
  , withArrow        :: Boolean
  )

tooltipFloatingToImpl :: TooltipFloatingProps -> TooltipFloatingPropsImpl
tooltipFloatingToImpl props =
  toNative (delete (Proxy :: Proxy "children") props)
    `union`
    -- wrap children in a explicit div to have forward ref on it
    { children: DOM.div_ [ props.children ] }

tooltipToImpl :: TooltipProps -> TooltipPropsImpl
tooltipToImpl props =
  toNative (delete (Proxy :: Proxy "children") props)
    `union`
    -- wrap children in a explicit div to have forward ref on it
    { children: DOM.div_ [ props.children ] }

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
