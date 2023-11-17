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
    ( chevron                :: Optional JSX
    , chevronPosition        :: AccordionChevronPosition
    , chevronSize            :: Optional Number
    , children               :: Array JSX
    , disableChevronRotation :: Boolean
    , id                     :: String
    , loop                   :: Boolean
    , order                  :: Optional AccordionOrder
    , radius                 :: MantineNumberSize
    , transitionDuration     :: Optional Milliseconds
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

type AccordionVariantImpl = OptionalImpl String

instance ToFFI AccordionVariant AccordionVariantImpl where
  toNative = toNative <<< Optional <<< case _ of
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
    ( chevron                :: OptionalImpl JSX
    , chevronPosition        :: AccordionChevronPositionImpl
    , chevronSize            :: OptionalImpl Number
    , children               :: Array JSX
    , disableChevronRotation :: Boolean
    , id                     :: String
    , loop                   :: Boolean
    , order                  :: OptionalImpl AccordionOrderImpl
    , radius                 :: MantineNumberSizeImpl
    , transitionDuration     :: OptionalImpl MillisecondsImpl
    , variant                :: AccordionVariantImpl
    | ControlledImpl accordionValueImpl
    )

accordionControl :: (AccordionControlProps -> AccordionControlProps) -> JSX
accordionControl = mkComponentWithDefault accordionControlComponent defaultValue

accordionControl_ :: Array JSX -> JSX
accordionControl_ children = accordionControl _ { children = children }

foreign import accordionControlComponent :: ReactComponent AccordionControlPropsImpl

type AccordionControlProps =
  { chevron  :: Optional JSX
  , children :: Array JSX
  , disabled :: Boolean
  , icon     :: Optional JSX
  }

type AccordionControlPropsImpl =
  { chevron  :: OptionalImpl JSX
  , children :: Array JSX
  , disabled :: Boolean
  , icon     :: OptionalImpl JSX
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
