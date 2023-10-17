module Mantine.Core.Navigation.Tabs
  ( tabs
  , tabs_
  , TabsProps
  , TabsPlacement(..)
  , TabsVariant(..)

  , tab
  , tab_
  , TabProps

  , tabList
  , tabList_
  , TabListProps
  , TabAlignment(..)

  , tabPanel
  , tabPanel_
  , TabPanelProps

  , module Mantine.Core.Common
  ) where

import Prelude (Unit)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Mantine.Core.Common (MantineColor, MantineNumberSize, MantineSize, Orientation)
import Mantine.Core.Common as MC
import Mantine.FFI (class ToFFI, toNative)
import React.Basic (ReactComponent, element)
import React.Basic.Hooks (JSX)

tabs :: (TabsProps -> TabsProps) -> JSX
tabs setProps = element tabsComponent (toNative (setProps MC.defaultThemingProps_))

tabs_ :: Array JSX -> JSX
tabs_ children = tabs _ { children = children}

foreign import tabsComponent :: ReactComponent TabsPropsImpl

type TabsProps =
  MC.ThemingProps
    ( activateTabWithKeyboard :: Boolean
    , allowTabDeactivation    :: Boolean
    , children                :: Array JSX
    , color                   :: Maybe MantineColor
    , defaultValue            :: Maybe String
    , id                      :: Maybe String
    , inverted                :: Boolean
    , keepMounted             :: Boolean
    , loop                    :: Boolean
    , onTabChange             :: Maybe (String -> Effect Unit)
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
  MC.ThemingPropsImpl
    ( activateTabWithKeyboard :: Boolean
    , allowTabDeactivation    :: Boolean
    , children                :: Array JSX
    , color                   :: Nullable String
    , defaultValue            :: Nullable String
    , id                      :: Nullable String
    , inverted                :: Boolean
    , keepMounted             :: Boolean
    , loop                    :: Boolean
    , onTabChange             :: Nullable (EffectFn1 String Unit)
    , orientation             :: Nullable String
    , placement               :: Nullable String
    , radius                  :: Nullable MC.MantineNumberSizeImpl
    , value                   :: Nullable String
    , variant                 :: Nullable String
    )

tab :: (TabProps -> TabProps) -> JSX
tab setProps = element tabComponent (toNative (setProps MC.defaultThemingProps_))

tab_ :: Array JSX -> JSX
tab_ children = tab _ { children = children}

foreign import tabComponent :: ReactComponent TabPropsImpl

type TabProps =
  MC.ThemingProps
    ( children     :: Array JSX
    , color        :: Maybe MantineColor
    , icon         :: Maybe JSX
    , rightSection :: Maybe JSX
    , value        :: Maybe String
    )

type TabPropsImpl =
  MC.ThemingPropsImpl
    ( children     :: Array JSX
    , color        :: Nullable String
    , icon         :: Nullable JSX
    , rightSection :: Nullable JSX
    , value        :: Nullable String
    )

tabList :: (TabListProps -> TabListProps) -> JSX
tabList setProps = element tabListComponent (toNative (setProps MC.defaultThemingProps_))

tabList_ :: Array JSX -> JSX
tabList_ children = tabList _ { children = children }

foreign import tabListComponent :: ReactComponent TabListPropsImpl

type TabListProps =
  MC.ThemingProps
    ( children :: Array JSX
    , grow     :: Boolean
    , position :: Maybe TabAlignment
    )

data TabAlignment
  = TabAlignmentLeft
  | TabAlignmentRight
  | TabAlignmentCenter
  | TabAlignmentApart

instance ToFFI TabAlignment String where
  toNative = case _ of
    TabAlignmentLeft   -> "left"
    TabAlignmentRight  -> "right"
    TabAlignmentCenter -> "center"
    TabAlignmentApart  -> "apart"

type TabListPropsImpl =
  MC.ThemingPropsImpl
    ( children :: Array JSX
    , grow     :: Boolean
    , position :: Nullable String
    )

tabPanel :: (TabPanelProps -> TabPanelProps) -> JSX
tabPanel setProps = element tabPanelComponent (toNative (setProps MC.defaultThemingProps_))

tabPanel_ :: Array JSX -> JSX
tabPanel_ children = tabPanel _ { children = children }

foreign import tabPanelComponent :: ReactComponent TabPanelPropsImpl

type TabPanelProps =
  MC.ThemingProps
    ( children :: Array JSX
    , value    :: Maybe String
    )

type TabPanelPropsImpl =
  MC.ThemingPropsImpl
    ( children :: Array JSX
    , value    :: Nullable String
    )
