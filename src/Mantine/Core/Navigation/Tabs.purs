module Mantine.Core.Navigation.Tabs
  ( tabs
  , tabs_
  , TabsProps
  , TabsPlacement(..)
  , TabsVariant(..)

  , tab
  , tab_
  , TabsTabProps

  , tabList
  , tabList_
  , TabListProps

  , tabPanel
  , tabPanel_
  , TabPanelProps
  ) where

import Mantine.Core.Prelude

tabs :: (TabsProps -> TabsProps) -> JSX
tabs = mkTrivialComponent tabsComponent

tabs_ :: Array JSX -> JSX
tabs_ children = tabs _ { children = children }

foreign import tabsComponent :: ReactComponent TabsPropsImpl

type TabsProps =
  MantineComponent
    ( activateTabWithKeyboard :: Boolean
    , allowTabDeactivation    :: Boolean
    , children                :: Array JSX
    , color                   :: Optional MantineColor
    , id                      :: Optional String
    , inverted                :: Boolean
    , keepMounted             :: Boolean
    , loop                    :: Boolean
    , orientation             :: Optional Orientation
    , placement               :: Optional TabsPlacement
    , radius                  :: Optional MantineNumberSize
    , variant                 :: Optional TabsVariant
    | Controlled String
    )

data TabsPlacement
  = TabsPlacementLeft
  | TabsPlacementRight

type TabsPlacementImpl = String

instance ToFFI TabsPlacement TabsPlacementImpl where
  toNative = case _ of
    TabsPlacementLeft  -> "left"
    TabsPlacementRight -> "right"

data TabsVariant
  = TabsVariantOutline
  | TabsVariantDefault
  | TabsVariantPills

type TabsVariantImpl = String

instance ToFFI TabsVariant TabsVariantImpl where
  toNative = case _ of
    TabsVariantOutline -> "outline"
    TabsVariantDefault -> "default"
    TabsVariantPills   -> "pills"

type TabsPropsImpl =
  MantineComponentImpl
    ( activateTabWithKeyboard :: Boolean
    , allowTabDeactivation    :: Boolean
    , children                :: Array JSX
    , color                   :: OptionalImpl MantineColorImpl
    , id                      :: OptionalImpl String
    , inverted                :: Boolean
    , keepMounted             :: Boolean
    , loop                    :: Boolean
    , orientation             :: OptionalImpl OrientationImpl
    , placement               :: OptionalImpl TabsPlacementImpl
    , radius                  :: OptionalImpl MantineNumberSizeImpl
    , variant                 :: OptionalImpl TabsVariantImpl
    | ControlledImpl String
    )

tab :: (TabsTabProps -> TabsTabProps) -> JSX
tab = mkTrivialComponent tabComponent

tab_ :: Array JSX -> JSX
tab_ children = tab _ { children = children}

foreign import tabComponent :: ReactComponent TabsTabPropsImpl

type TabsTabProps =
  MantineComponent
    ( children     :: Array JSX
    , color        :: Optional MantineColor
    , leftSection  :: Optional JSX
    , rightSection :: Optional JSX
    , size         :: Optional MantineNumberSize
    , value        :: Optional String
    )

type TabsTabPropsImpl =
  MantineComponentImpl
    ( children     :: Array JSX
    , color        :: OptionalImpl MantineColorImpl
    , leftSection  :: OptionalImpl JSX
    , rightSection :: OptionalImpl JSX
    , size         :: OptionalImpl MantineNumberSizeImpl
    , value        :: OptionalImpl String
    )

tabList :: (TabListProps -> TabListProps) -> JSX
tabList = mkTrivialComponent tabListComponent

tabList_ :: Array JSX -> JSX
tabList_ children = tabList _ { children = children }

foreign import tabListComponent :: ReactComponent TabListPropsImpl

type TabListProps =
  MantineComponent
    ( children :: Array JSX
    , grow     :: Boolean
    , justify  :: Optional JustifyContent
    )

type TabListPropsImpl =
  MantineComponentImpl
    ( children :: Array JSX
    , grow     :: Boolean
    , justify  :: OptionalImpl JustifyContentImpl
    )

tabPanel :: (TabPanelProps -> TabPanelProps) -> JSX
tabPanel = mkTrivialComponent tabPanelComponent

tabPanel_ :: Array JSX -> JSX
tabPanel_ children = tabPanel _ { children = children }

foreign import tabPanelComponent :: ReactComponent TabPanelPropsImpl

type TabPanelProps =
  MantineComponent
    ( children :: Array JSX
    , value    :: String
    )

type TabPanelPropsImpl =
  MantineComponentImpl
    ( children :: Array JSX
    , value    :: String
    )
