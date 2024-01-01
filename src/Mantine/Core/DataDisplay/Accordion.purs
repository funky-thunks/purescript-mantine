module Mantine.Core.DataDisplay.Accordion
  ( accordion
  , multipleAccordion
  , Props_Accordion
  , Props_AccordionImpl
  , AccordionChevronPosition(..)
  , AccordionChevronPositionImpl
  , AccordionOrder(..)
  , AccordionOrderImpl
  , AccordionVariant(..)
  , AccordionVariantImpl

  , accordionControl
  , accordionControl_
  , Props_AccordionControl
  , Props_AccordionControlImpl

  , accordionItem
  , accordionItem_
  , Props_AccordionItem
  , Props_AccordionItemImpl

  , accordionPanel_
  ) where

import Mantine.Core.Prelude

accordion
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     (Props_Accordion     String)
  => Union attrsImpl attrsImpl_ (Props_AccordionImpl String)
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
accordion = element (unsafeCoerce accordionComponent) <<< toNative

multipleAccordion
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     (Props_Accordion     (Array String))
  => Union attrsImpl attrsImpl_ (Props_AccordionImpl (Array String))
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
multipleAccordion = element (unsafeCoerce multipleAccordionComponent) <<< toNative

foreign import accordionComponent :: ReactComponent (Record (Props_AccordionImpl String))

foreign import multipleAccordionComponent :: ReactComponent (Record (Props_AccordionImpl (Array String)))

type Props_Accordion accordionValue =
  Props_Common
    ( chevron                :: JSX
    , chevronPosition        :: AccordionChevronPosition
    , chevronSize            :: Number
    , children               :: Array JSX
    , disableChevronRotation :: Boolean
    , id                     :: String
    , loop                   :: Boolean
    , order                  :: AccordionOrder
    , radius                 :: MantineNumberSize
    , transitionDuration     :: Milliseconds
    , variant                :: AccordionVariant
    | Controlled ValueHandler accordionValue
    )

data AccordionChevronPosition
  = AccordionChevronPositionLeft
  | AccordionChevronPositionRight

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

type Props_AccordionImpl accordionValueImpl =
  Props_CommonImpl
    ( chevron                :: JSX
    , chevronPosition        :: AccordionChevronPositionImpl
    , chevronSize            :: Number
    , children               :: Array JSX
    , disableChevronRotation :: Boolean
    , id                     :: String
    , loop                   :: Boolean
    , order                  :: AccordionOrderImpl
    , radius                 :: MantineNumberSizeImpl
    , transitionDuration     :: MillisecondsImpl
    , variant                :: AccordionVariantImpl
    | ControlledImpl ValueHandlerImpl accordionValueImpl
    )

accordionControl
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_AccordionControl
  => Union attrsImpl attrsImpl_ Props_AccordionControlImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
accordionControl = element (unsafeCoerce accordionControlComponent) <<< toNative

accordionControl_ :: Array JSX -> JSX
accordionControl_ children = accordionControl { children }

foreign import accordionControlComponent :: ReactComponent (Record Props_AccordionControlImpl)

type Props_AccordionControl =
  ( chevron  :: JSX
  , children :: Array JSX
  , disabled :: Boolean
  , icon     :: JSX
  )

type Props_AccordionControlImpl =
  ( chevron  :: JSX
  , children :: Array JSX
  , disabled :: Boolean
  , icon     :: JSX
  )

accordionItem
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_AccordionItem
  => Union attrsImpl attrsImpl_ Props_AccordionItemImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
accordionItem = element (unsafeCoerce accordionItemComponent) <<< toNative

accordionItem_ :: Array JSX -> JSX
accordionItem_ children = accordionItem { children }

foreign import accordionItemComponent :: ReactComponent (Record Props_AccordionItemImpl)

type Props_AccordionItem =
  ( children :: Array JSX
  , value    :: String
  )

type Props_AccordionItemImpl =
  ( children :: Array JSX
  , value    :: String
  )

accordionPanel
  :: forall attrs attrs_
   . Union attrs attrs_ Props_AccordionPanel
  => Record attrs -> JSX
accordionPanel = element (unsafeCoerce accordionPanelComponent)

accordionPanel_ :: Array JSX -> JSX
accordionPanel_ children = accordionPanel { children }

foreign import accordionPanelComponent :: ReactComponent (Record Props_AccordionPanel)

type Props_AccordionPanel = ( children :: Array JSX )
