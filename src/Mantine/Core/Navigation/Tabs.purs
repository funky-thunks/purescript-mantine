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
  ThemingProps
    ( activateTabWithKeyboard :: Boolean
    , allowTabDeactivation    :: Boolean
    , children                :: Array JSX
    , color                   :: Maybe MantineColor
    , defaultValue            :: Maybe String
    , id                      :: Maybe String
    , inverted                :: Boolean
    , keepMounted             :: Boolean
    , loop                    :: Boolean
    , onChange                :: Maybe (String -> Effect Unit)
    , orientation             :: Maybe Orientation
    , placement               :: Maybe TabsPlacement
    , radius                  :: Maybe MantineNumberSize
    , value                   :: Maybe String
    , variant                 :: Maybe TabsVariant
    )

data TabsPlacement
  = TabsPlacementLeft
  | TabsPlacementRight

instance ToFFI TabsPlacement String where
  toNative = case _ of
    TabsPlacementLeft  -> "left"
    TabsPlacementRight -> "right"

data TabsVariant
  = TabsVariantOutline
  | TabsVariantDefault
  | TabsVariantPills

instance ToFFI TabsVariant String where
  toNative = case _ of
    TabsVariantOutline -> "outline"
    TabsVariantDefault -> "default"
    TabsVariantPills   -> "pills"

type TabsPropsImpl =
  ThemingPropsImpl
    ( activateTabWithKeyboard :: Boolean
    , allowTabDeactivation    :: Boolean
    , children                :: Array JSX
    , color                   :: Nullable String
    , defaultValue            :: Nullable String
    , id                      :: Nullable String
    , inverted                :: Boolean
    , keepMounted             :: Boolean
    , loop                    :: Boolean
    , onChange                :: Nullable (EffectFn1 String Unit)
    , orientation             :: Nullable String
    , placement               :: Nullable String
    , radius                  :: Nullable MantineNumberSizeImpl
    , value                   :: Nullable String
    , variant                 :: Nullable String
    )

tab :: (TabsTabProps -> TabsTabProps) -> JSX
tab = mkTrivialComponent tabComponent

tab_ :: Array JSX -> JSX
tab_ children = tab _ { children = children}

foreign import tabComponent :: ReactComponent TabsTabPropsImpl

type TabsTabProps =
  ThemingProps
    ( children     :: Array JSX
    , color        :: Maybe MantineColor
    , leftSection  :: Maybe JSX
    , rightSection :: Maybe JSX
    , size         :: Maybe MantineNumberSize
    , value        :: Maybe String
    )

type TabsTabPropsImpl =
  ThemingPropsImpl
    ( children     :: Array JSX
    , color        :: Nullable String
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
  ThemingProps
    ( children :: Array JSX
    , grow     :: Boolean
    , justify  :: Maybe JustifyContent
    )

type TabListPropsImpl =
  ThemingPropsImpl
    ( children :: Array JSX
    , grow     :: Boolean
    , justify  :: Nullable String
    )

tabPanel :: (TabPanelProps -> TabPanelProps) -> JSX
tabPanel = mkTrivialComponent tabPanelComponent

tabPanel_ :: Array JSX -> JSX
tabPanel_ children = tabPanel _ { children = children }

foreign import tabPanelComponent :: ReactComponent TabPanelPropsImpl

type TabPanelProps =
  ThemingProps
    ( children :: Array JSX
    , value    :: String
    )

type TabPanelPropsImpl =
  ThemingPropsImpl
    ( children :: Array JSX
    , value    :: String
    )
