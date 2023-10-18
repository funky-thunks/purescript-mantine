module Mantine.Core.Overlays.HoverCard
  ( hoverCard
  , HoverCardProps
  , HoverArrowPosition(..)
  , HoverFloatingPosition(..)
  , HoverPopoverWidth(..)

  , hoverCardTarget
  , HoverCardTargetProps

  , hoverCardDropdown
  , HoverCardDropdownProps
  ) where

import Prelude
import Mantine.Core.Prelude
import React.Basic.DOM as DOM

hoverCard :: (HoverCardProps -> HoverCardProps) -> JSX
hoverCard = mkComponent hoverCardComponent hoverCardToImpl defaultHoverCardProps

foreign import hoverCardComponent :: ReactComponent HoverCardPropsImpl

defaultHoverCardProps :: HoverCardProps
defaultHoverCardProps =
  defaultThemingProps
  { onClose: pure unit, onOpen: pure unit } `union` defaultValue

hoverCardToImpl :: HoverCardProps -> HoverCardPropsImpl
hoverCardToImpl = toNative

type HoverCardProps =
  ThemingProps
    ( children :: Array JSX
    , arrowOffset            :: Maybe Number
    , arrowPosition          :: Maybe HoverArrowPosition
    , arrowRadius            :: Maybe Number
    , arrowSize              :: Maybe Number
    , closeDelay             :: Maybe Number
    , disabled               :: Boolean
    , exitTransitionDuration :: Maybe Number
    , initiallyOpened        :: Maybe Boolean
 -- , middlewares            :: PopoverMiddlewares -- TODO
    , offset                 :: Maybe Number
    , onClose                :: Effect Unit
    , onOpen                 :: Effect Unit
    , onPositionChange       :: ValueHandler HoverFloatingPosition
    , openDelay              :: Maybe Number
    , position               :: HoverFloatingPosition
 -- , positionDependencies   :: any[] -- TODO
    , radius                 :: Maybe MantineNumberSize
    , returnFocus            :: Boolean
 -- , shadow                 :: MantineShadow -- TODO
    , transition             :: Maybe MantineTransition
    , transitionDuration     :: Maybe Number
    , width                  :: HoverPopoverWidth
    , withArrow              :: Boolean
    , withinPortal           :: Boolean
    , zIndex                 :: Maybe Number
    )

data HoverFloatingPosition
  = HoverFloatingPositionTop      | HoverFloatingPositionRight      | HoverFloatingPositionBottom      | HoverFloatingPositionLeft
  | HoverFloatingPositionTopEnd   | HoverFloatingPositionRightEnd   | HoverFloatingPositionBottomEnd   | HoverFloatingPositionLeftEnd
  | HoverFloatingPositionTopStart | HoverFloatingPositionRightStart | HoverFloatingPositionBottomStart | HoverFloatingPositionLeftStart

instance DefaultValue HoverFloatingPosition where defaultValue = HoverFloatingPositionBottom

instance ToFFI HoverFloatingPosition String where
  toNative = case _ of
    HoverFloatingPositionTop         -> "top"
    HoverFloatingPositionRight       -> "right"
    HoverFloatingPositionBottom      -> "bottom"
    HoverFloatingPositionLeft        -> "left"
    HoverFloatingPositionTopEnd      -> "top-end"
    HoverFloatingPositionRightEnd    -> "right-end"
    HoverFloatingPositionBottomEnd   -> "bottom-end"
    HoverFloatingPositionLeftEnd     -> "left-end"
    HoverFloatingPositionTopStart    -> "top-start"
    HoverFloatingPositionRightStart  -> "right-start"
    HoverFloatingPositionBottomStart -> "bottom-start"
    HoverFloatingPositionLeftStart   -> "left-start"

instance FromFFI String HoverFloatingPosition where
  fromNative = case _ of
    "top"          -> HoverFloatingPositionTop
    "right"        -> HoverFloatingPositionRight
    "bottom"       -> HoverFloatingPositionBottom
    "left"         -> HoverFloatingPositionLeft
    "top-end"      -> HoverFloatingPositionTopEnd
    "right-end"    -> HoverFloatingPositionRightEnd
    "bottom-end"   -> HoverFloatingPositionBottomEnd
    "left-end"     -> HoverFloatingPositionLeftEnd
    "top-start"    -> HoverFloatingPositionTopStart
    "right-start"  -> HoverFloatingPositionRightStart
    "bottom-start" -> HoverFloatingPositionBottomStart
    "left-start"   -> HoverFloatingPositionLeftStart
    _              -> defaultValue

data HoverPopoverWidth = AsTarget | Fixed Number

instance DefaultValue HoverPopoverWidth where defaultValue = AsTarget

instance ToFFI HoverPopoverWidth (String |+| Number) where
  toNative = case _ of
    AsTarget -> asOneOf "target"
    Fixed n  -> asOneOf n

type HoverCardPropsImpl =
  ThemingPropsImpl
    ( children               :: Array JSX
    , arrowOffset            :: Nullable Number
    , arrowPosition          :: Nullable String
    , arrowRadius            :: Nullable Number
    , arrowSize              :: Nullable Number
    , closeDelay             :: Nullable Number
    , disabled               :: Boolean
    , exitTransitionDuration :: Nullable Number
    , initiallyOpened        :: Nullable Boolean
 -- , middlewares            :: PopoverMiddlewares -- TODO
    , offset                 :: Nullable Number
    , onClose                :: Effect Unit
    , onOpen                 :: Effect Unit
    , onPositionChange       :: EffectFn1 String Unit
    , openDelay              :: Nullable Number
    , position               :: String
 -- , positionDependencies   :: any[] -- TODO
    , radius                 :: Nullable MantineNumberSizeImpl
    , returnFocus            :: Boolean
 -- , shadow                 :: MantineShadow -- TODO
    , transition             :: Nullable String
    , transitionDuration     :: Nullable Number
    , width                  :: String |+| Number
    , withArrow              :: Boolean
    , withinPortal           :: Boolean
    , zIndex                 :: Nullable Number
    )

data HoverArrowPosition = HoverArrowPositionCenter | HoverArrowPositionSide

instance ToFFI HoverArrowPosition String where
  toNative = case _ of
    HoverArrowPositionCenter -> "center"
    HoverArrowPositionSide   -> "side"

hoverCardTarget :: (HoverCardTargetProps -> HoverCardTargetProps) -> JSX
hoverCardTarget = mkComponent hoverCardTargetComponent hoverCardTargetToImpl defaultHoverCardTargetProps

foreign import hoverCardTargetComponent :: ReactComponent HoverCardTargetPropsImpl

defaultHoverCardTargetProps :: HoverCardTargetProps
defaultHoverCardTargetProps = defaultThemingProps_

hoverCardTargetToImpl :: HoverCardTargetProps -> HoverCardTargetPropsImpl
hoverCardTargetToImpl =
  let wrapChildren props = props { children = [ DOM.div_ props.children ] }
   in toNative >>> wrapChildren

type HoverCardTargetProps =
  ThemingProps
    ( children  :: Array JSX
    , popupType :: Maybe String
    , refProps  :: Maybe String
    )

type HoverCardTargetPropsImpl =
  ThemingPropsImpl
    ( children  :: Array JSX
    , popupType :: Nullable String
    , refProps  :: Nullable String
    )

hoverCardDropdown :: (HoverCardDropdownProps -> HoverCardDropdownProps) -> JSX
hoverCardDropdown = mkComponent hoverCardDropdownComponent hoverCardDropdownToImpl defaultHoverCardDropdownProps

foreign import hoverCardDropdownComponent :: ReactComponent HoverCardDropdownPropsImpl

defaultHoverCardDropdownProps :: HoverCardDropdownProps
defaultHoverCardDropdownProps = defaultThemingProps_

hoverCardDropdownToImpl :: HoverCardDropdownProps -> HoverCardDropdownPropsImpl
hoverCardDropdownToImpl = toNative

type HoverCardDropdownProps =
  ThemingProps
    ( children :: Array JSX
    )

type HoverCardDropdownPropsImpl =
  ThemingPropsImpl
    ( children :: Array JSX
    )
