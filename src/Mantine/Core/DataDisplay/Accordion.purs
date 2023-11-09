module Mantine.Core.DataDisplay.Accordion
  ( accordion
  , multipleAccordion
  , AccordionProps
  , AccordionChevronPosition(..)
  , AccordionOrder(..)
  , AccordionVariant(..)

  , accordionControl
  , accordionControl_
  , AccordionControlProps

  , accordionItem
  , accordionItem_
  , AccordionItemProps

  , accordionPanel_
  ) where

import Mantine.Core.Prelude

accordion :: (AccordionProps String -> AccordionProps String) -> JSX
accordion = mkComponentWithDefault accordionComponent defaultAccordionProps

multipleAccordion :: (AccordionProps (Array String) -> AccordionProps (Array String)) -> JSX
multipleAccordion = mkComponentWithDefault multipleAccordionComponent defaultAccordionProps

foreign import accordionComponent :: ReactComponent (AccordionPropsImpl String)

foreign import multipleAccordionComponent :: ReactComponent (AccordionPropsImpl (Array String))

defaultAccordionProps :: forall accordionValue. AccordionProps accordionValue
defaultAccordionProps = defaultMantineComponent { radius: Preset Small }

type AccordionProps accordionValue =
  MantineComponent
    ( chevron                :: Maybe JSX
    , chevronPosition        :: AccordionChevronPosition
    , chevronSize            :: Maybe Number
    , children               :: Array JSX
    , disableChevronRotation :: Boolean
    , id                     :: String
    , loop                   :: Boolean
    , order                  :: Maybe AccordionOrder
    , radius                 :: MantineNumberSize
    , transitionDuration     :: Maybe Milliseconds
    , variant                :: AccordionVariant
    | Controlled accordionValue
    )

data AccordionChevronPosition
  = AccordionChevronPositionLeft
  | AccordionChevronPositionRight

instance DefaultValue AccordionChevronPosition where
  defaultValue = AccordionChevronPositionRight

type AccordionChevronPositionImpl = String

instance ToFFI AccordionChevronPosition AccordionChevronPositionImpl where
  toNative = case _ of
    AccordionChevronPositionLeft  -> "left"
    AccordionChevronPositionRight -> "right"

data AccordionVariant
  = AccordionVariantDefault
  | AccordionVariantContained
  | AccordionVariantFilled
  | AccordionVariantSeparated

instance DefaultValue AccordionVariant where
  defaultValue = AccordionVariantDefault

type AccordionVariantImpl = Nullable String

instance ToFFI AccordionVariant AccordionVariantImpl where
  toNative = toNative <<< case _ of
    AccordionVariantDefault   -> Nothing
    AccordionVariantContained -> Just "contained"
    AccordionVariantFilled    -> Just "filled"
    AccordionVariantSeparated -> Just "separated"

data AccordionOrder
  = AccordionOrder2
  | AccordionOrder3
  | AccordionOrder4
  | AccordionOrder5
  | AccordionOrder6

type AccordionOrderImpl = Int

instance ToFFI AccordionOrder AccordionOrderImpl where
  toNative = case _ of
    AccordionOrder2 -> 2
    AccordionOrder3 -> 3
    AccordionOrder4 -> 4
    AccordionOrder5 -> 5
    AccordionOrder6 -> 6

type AccordionPropsImpl accordionValueImpl =
  MantineComponentImpl
    ( chevron                :: Nullable JSX
    , chevronPosition        :: AccordionChevronPositionImpl
    , chevronSize            :: Nullable Number
    , children               :: Array JSX
    , disableChevronRotation :: Boolean
    , id                     :: String
    , loop                   :: Boolean
    , order                  :: Nullable AccordionOrderImpl
    , radius                 :: MantineNumberSizeImpl
    , transitionDuration     :: Nullable MillisecondsImpl
    , variant                :: AccordionVariantImpl
    | ControlledImpl accordionValueImpl
    )

accordionControl :: (AccordionControlProps -> AccordionControlProps) -> JSX
accordionControl = mkComponentWithDefault accordionControlComponent defaultValue

accordionControl_ :: Array JSX -> JSX
accordionControl_ children = accordionControl _ { children = children }

foreign import accordionControlComponent :: ReactComponent AccordionControlPropsImpl

type AccordionControlProps =
  { chevron  :: Maybe JSX
  , children :: Array JSX
  , disabled :: Boolean
  , icon     :: Maybe JSX
  }

type AccordionControlPropsImpl =
  { chevron  :: Nullable JSX
  , children :: Array JSX
  , disabled :: Boolean
  , icon     :: Nullable JSX
  }

accordionItem :: (AccordionItemProps -> AccordionItemProps) -> JSX
accordionItem = mkComponentWithDefault accordionItemComponent defaultValue

accordionItem_ :: Array JSX -> JSX
accordionItem_ children = accordionItem _ { children = children }

foreign import accordionItemComponent :: ReactComponent AccordionItemPropsImpl

type AccordionItemProps =
  { children :: Array JSX
  , value    :: String
  }

type AccordionItemPropsImpl =
  { children :: Array JSX
  , value    :: String
  }

accordionPanel :: (AccordionPanelProps -> AccordionPanelProps) -> JSX
accordionPanel = mkComponentWithDefault accordionPanelComponent defaultValue

accordionPanel_ :: Array JSX -> JSX
accordionPanel_ children = accordionPanel _ { children = children }

foreign import accordionPanelComponent :: ReactComponent AccordionPanelProps

type AccordionPanelProps =
  { children :: Array JSX
  }
