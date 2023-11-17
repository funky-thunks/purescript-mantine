module Mantine.Core.Navigation.Tabs
  ( tabs
  , tabs_
  , Props_Tabs
  , Props_TabsImpl
  , TabsPlacement(..)
  , TabsPlacementImpl
  , TabsVariant(..)
  , TabsVariantImpl

  , tab
  , tab_
  , Props_TabsTab
  , Props_TabsTabImpl

  , tabList
  , tabList_
  , Props_TabList
  , Props_TabListImpl

  , tabPanel
  , tabPanel_
  , Props_TabPanel
  , Props_TabPanelImpl
  ) where

import Mantine.Core.Prelude

tabs
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Tabs
  => Union attrsImpl attrsImpl_ Props_TabsImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
tabs = element (unsafeCoerce tabsComponent) <<< toNative

tabs_ :: Array JSX -> JSX
tabs_ children = tabs { children }

foreign import tabsComponent :: ReactComponent (Record Props_TabsImpl)

type Props_Tabs =
  Props_Common
    ( activateTabWithKeyboard :: Boolean
    , allowTabDeactivation    :: Boolean
    , children                :: Array JSX
    , color                   :: MantineColor
    , id                      :: String
    , inverted                :: Boolean
    , keepMounted             :: Boolean
    , loop                    :: Boolean
    , orientation             :: Orientation
    , placement               :: TabsPlacement
    , radius                  :: MantineNumberSize
    , variant                 :: TabsVariant
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

type Props_TabsImpl =
  Props_CommonImpl
    ( activateTabWithKeyboard :: Boolean
    , allowTabDeactivation    :: Boolean
    , children                :: Array JSX
    , color                   :: MantineColorImpl
    , id                      :: String
    , inverted                :: Boolean
    , keepMounted             :: Boolean
    , loop                    :: Boolean
    , orientation             :: OrientationImpl
    , placement               :: TabsPlacementImpl
    , radius                  :: MantineNumberSizeImpl
    , variant                 :: TabsVariantImpl
    | ControlledImpl String
    )

tab
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_TabsTab
  => Union attrsImpl attrsImpl_ Props_TabsTabImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
tab = element (unsafeCoerce tabComponent) <<< toNative

tab_ :: Array JSX -> JSX
tab_ children = tab { children }

foreign import tabComponent :: ReactComponent (Record Props_TabsTabImpl)

type Props_TabsTab =
  Props_Common
    ( children     :: Array JSX
    , color        :: MantineColor
    , leftSection  :: JSX
    , rightSection :: JSX
    , size         :: MantineNumberSize
    , value        :: String
    )

type Props_TabsTabImpl =
  Props_CommonImpl
    ( children     :: Array JSX
    , color        :: MantineColorImpl
    , leftSection  :: JSX
    , rightSection :: JSX
    , size         :: MantineNumberSizeImpl
    , value        :: String
    )

tabList
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_TabList
  => Union attrsImpl attrsImpl_ Props_TabListImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
tabList = element (unsafeCoerce tabListComponent) <<< toNative

tabList_ :: Array JSX -> JSX
tabList_ children = tabList { children }

foreign import tabListComponent :: ReactComponent (Record Props_TabListImpl)

type Props_TabList =
  Props_Common
    ( children :: Array JSX
    , grow     :: Boolean
    , justify  :: JustifyContent
    )

type Props_TabListImpl =
  Props_CommonImpl
    ( children :: Array JSX
    , grow     :: Boolean
    , justify  :: JustifyContentImpl
    )

tabPanel
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_TabPanel
  => Union attrsImpl attrsImpl_ Props_TabPanelImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
tabPanel = element (unsafeCoerce tabPanelComponent) <<< toNative

tabPanel_ :: Array JSX -> JSX
tabPanel_ children = tabPanel { children }

foreign import tabPanelComponent :: ReactComponent (Record Props_TabPanelImpl)

type Props_TabPanel =
  Props_Common
    ( children :: Array JSX
    , value    :: String
    )

type Props_TabPanelImpl =
  Props_CommonImpl
    ( children :: Array JSX
    , value    :: String
    )
