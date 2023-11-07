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
defaultAccordionProps = defaultThemingProps { radius: Preset Small }

type AccordionProps accordionValue =
  ThemingProps
    ( chevron                :: Maybe JSX
    , chevronPosition        :: AccordionChevronPosition
    , chevronSize            :: Maybe Number
    , children               :: Array JSX
    , defaultValue           :: Maybe accordionValue
    , disableChevronRotation :: Boolean
    , id                     :: String
    , loop                   :: Boolean
    , onChange               :: ValueHandler accordionValue
    , order                  :: Maybe AccordionOrder
    , radius                 :: MantineNumberSize
    , transitionDuration     :: Maybe Number
    , value                  :: Maybe accordionValue
    , variant                :: AccordionVariant
    )

data AccordionChevronPosition = AccordionChevronPositionLeft | AccordionChevronPositionRight

instance DefaultValue AccordionChevronPosition where defaultValue = AccordionChevronPositionRight

instance ToFFI AccordionChevronPosition String where
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

instance ToFFI AccordionVariant (Nullable String) where
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

instance ToFFI AccordionOrder Int where
  toNative = case _ of
    AccordionOrder2 -> 2
    AccordionOrder3 -> 3
    AccordionOrder4 -> 4
    AccordionOrder5 -> 5
    AccordionOrder6 -> 6

type AccordionPropsImpl accordionValueNative =
  ThemingPropsImpl
    ( chevron                :: Nullable JSX
    , chevronPosition        :: String
    , chevronSize            :: Nullable Number
    , children               :: Array JSX
    , defaultValue           :: Nullable accordionValueNative
    , disableChevronRotation :: Boolean
    , id                     :: String
    , loop                   :: Boolean
    , onChange               :: EffectFn1 accordionValueNative Unit
    , order                  :: Nullable Int
    , radius                 :: MantineNumberSizeImpl
    , transitionDuration     :: Nullable Number
    , value                  :: Nullable accordionValueNative
    , variant                :: Nullable String
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
