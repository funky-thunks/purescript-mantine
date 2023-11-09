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
    , color                   :: Maybe MantineColor
    , id                      :: Maybe String
    , inverted                :: Boolean
    , keepMounted             :: Boolean
    , loop                    :: Boolean
    , orientation             :: Maybe Orientation
    , placement               :: Maybe TabsPlacement
    , radius                  :: Maybe MantineNumberSize
    , variant                 :: Maybe TabsVariant
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
    , color                   :: Nullable MantineColorImpl
    , id                      :: Nullable String
    , inverted                :: Boolean
    , keepMounted             :: Boolean
    , loop                    :: Boolean
    , orientation             :: Nullable OrientationImpl
    , placement               :: Nullable TabsPlacementImpl
    , radius                  :: Nullable MantineNumberSizeImpl
    , variant                 :: Nullable TabsVariantImpl
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
    , color        :: Maybe MantineColor
    , leftSection  :: Maybe JSX
    , rightSection :: Maybe JSX
    , size         :: Maybe MantineNumberSize
    , value        :: Maybe String
    )

type TabsTabPropsImpl =
  MantineComponentImpl
    ( children     :: Array JSX
    , color        :: Nullable MantineColorImpl
    , leftSection  :: Nullable JSX
    , rightSection :: Nullable JSX
    , size         :: Nullable MantineNumberSizeImpl
    , value        :: Nullable String
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
    , justify  :: Maybe JustifyContent
    )

type TabListPropsImpl =
  MantineComponentImpl
    ( children :: Array JSX
    , grow     :: Boolean
    , justify  :: Nullable JustifyContentImpl
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
