module Mantine.Core.Layout.AppShell
  ( appShell
  , Props_AppShell
  , Props_AppShellImpl
  , AppShellCollapse
  , AppShellHorizontalConfiguration
  , AppShellHorizontalConfigurationImpl
  , AppShellLayout(..)
  , AppShellPadding(..)
  , AppShellPaddingImpl
  , AppShellResponsiveSize
  , AppShellRules(..)
  , AppShellRulesImpl
  , AppShellSize(..)
  , AppShellSizeImpl
  , AppShellVerticalConfiguration
  , AppShellVerticalConfigurationImpl

  , appShellMain
  , Props_AppShellMain
  , Props_AppShellMainImpl

  , appShellSection
  , appShellSection_
  , appShellScrollableSection
  , appShellScrollableSection_
  , Props_AppShellSection
  , Props_AppShellSectionImpl

  -- AppShell Components
  , Props_AppShellComponent
  , Props_AppShellComponentImpl
  , appShellNavbar
  , appShellNavbar_

  , appShellHeader
  , appShellHeader_

  , appShellAside
  , appShellAside_

  , appShellFooter
  , appShellFooter_
  ) where

import Mantine.Core.Prelude

appShell
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_AppShell
  => Union attrsImpl attrsImpl_ Props_AppShellImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
appShell = element (unsafeCoerce appShellComponent) <<< toNative

foreign import appShellComponent :: ReactComponent (Record Props_AppShellImpl)

type Props_AppShell =
  Props_Common
    ( aside                    :: AppShellVerticalConfiguration
    , disabled                 :: Boolean
    , footer                   :: AppShellHorizontalConfiguration
    , header                   :: AppShellHorizontalConfiguration
    , layout                   :: AppShellLayout
    , navbar                   :: AppShellVerticalConfiguration
    , offsetScrollbars         :: Boolean
    , padding                  :: AppShellPadding
    , transitionDuration       :: Milliseconds
    , transitionTimingFunction :: MantineTransitionTimingFunction
    , withBorder               :: Boolean
    , zIndex                   :: ZIndex
    )

data AppShellLayout
  = AppShellLayoutDefault
  | AppShellLayoutAlt

instance ToFFI AppShellLayout String where
  toNative = case _ of
    AppShellLayoutDefault -> "default"
    AppShellLayoutAlt     -> "alt"

data AppShellPadding
  = FixedPadding MantineNumberSize
  | ResponsivePadding AppShellResponsiveSize

type AppShellPaddingImpl = MantineNumberSizeImpl |+| AppShellResponsiveSize

instance ToFFI AppShellPadding AppShellPaddingImpl where
  toNative = case _ of
    FixedPadding      f -> asOneOf (toNative f)
    ResponsivePadding r -> asOneOf r

type AppShellHorizontalConfiguration =
  { height    :: AppShellSize
  , collapsed :: Boolean
  , offset    :: Boolean
  }

data AppShellSize
  = AppShellSizeFixed      Pixels
  | AppShellSizeResponsive AppShellResponsiveSize

type AppShellResponsiveSize = Object Number

type AppShellSizeImpl = Number |+| AppShellResponsiveSize

instance ToFFI AppShellSize AppShellSizeImpl where
  toNative = case _ of
    AppShellSizeFixed      s  -> asOneOf s
    AppShellSizeResponsive rs -> asOneOf rs

type AppShellVerticalConfiguration =
  { width      :: AppShellSize
  , breakpoint :: MantineNumberSize
  , collapsed  :: AppShellCollapse
  }

type AppShellCollapse = { desktop :: Boolean, mobile :: Boolean }

type Props_AppShellImpl =
  Props_CommonImpl
    ( aside                    :: AppShellVerticalConfigurationImpl
    , disabled                 :: Boolean
    , footer                   :: AppShellHorizontalConfigurationImpl
    , header                   :: AppShellHorizontalConfigurationImpl
    , layout                   :: String
    , navbar                   :: AppShellVerticalConfigurationImpl
    , offsetScrollbars         :: Boolean
    , padding                  :: AppShellPaddingImpl
    , transitionDuration       :: MillisecondsImpl
    , transitionTimingFunction :: MantineTransitionTimingFunctionImpl
    , withBorder               :: Boolean
    , zIndex                   :: ZIndexImpl
    )

type AppShellHorizontalConfigurationImpl =
  { height    :: AppShellSizeImpl
  , collapsed :: Boolean
  , offset    :: Boolean
  }

type AppShellVerticalConfigurationImpl =
  { width      :: AppShellSizeImpl
  , breakpoint :: MantineNumberSizeImpl
  , collapsed  :: AppShellCollapse
  }

newtype AppShellRules = AppShellRules (Array (String /\ Either String Number))

type AppShellRulesImpl = Object (String |+| Number)

instance ToFFI AppShellRules AppShellRulesImpl where
  toNative (AppShellRules rs) =
    let f = either asOneOf asOneOf
     in fromFoldable (map f <$> rs)

appShellSection
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_AppShellSection
  => Union attrsImpl attrsImpl_ Props_AppShellSectionImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
appShellSection = element (unsafeCoerce appShellSectionComponent) <<< toNative

appShellSection_ :: Array JSX -> JSX
appShellSection_ children = appShellSection { children }

foreign import appShellSectionComponent :: ReactComponent (Record Props_AppShellSectionImpl)

appShellScrollableSection
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_AppShellSection
  => Union attrsImpl attrsImpl_ Props_AppShellSectionImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
appShellScrollableSection = element (unsafeCoerce appShellScrollableSectionComponent) <<< toNative

appShellScrollableSection_ :: Array JSX -> JSX
appShellScrollableSection_ children = appShellScrollableSection { children }

foreign import appShellScrollableSectionComponent :: ReactComponent (Record Props_AppShellSectionImpl)

type Props_AppShellSection =
  Props_Common
    ( children :: Array JSX
    , grow     :: Boolean
    )

type Props_AppShellSectionImpl =
  Props_CommonImpl
    ( children :: Array JSX
    , grow     :: Boolean
    )

appShellMain
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_AppShellMain
  => Union attrsImpl attrsImpl_ Props_AppShellMainImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
appShellMain = element (unsafeCoerce appShellMainComponent) <<< toNative

foreign import appShellMainComponent :: ReactComponent (Record Props_AppShellMainImpl)

type Props_AppShellMain     = Props_Common     ( children :: Array JSX )
type Props_AppShellMainImpl = Props_CommonImpl ( children :: Array JSX )

appShellNavbar
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_AppShellComponent
  => Union attrsImpl attrsImpl_ Props_AppShellComponentImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
appShellNavbar = element (unsafeCoerce appShellNavbarComponent) <<< toNative

appShellNavbar_ :: Array JSX -> JSX
appShellNavbar_ children = appShellNavbar { children }

foreign import appShellNavbarComponent :: ReactComponent (Record Props_AppShellComponentImpl)

appShellHeader
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_AppShellComponent
  => Union attrsImpl attrsImpl_ Props_AppShellComponentImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
appShellHeader = element (unsafeCoerce appShellHeaderComponent) <<< toNative

appShellHeader_ :: Array JSX -> JSX
appShellHeader_ children = appShellHeader { children }

foreign import appShellHeaderComponent :: ReactComponent (Record Props_AppShellComponentImpl)

appShellAside
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_AppShellComponent
  => Union attrsImpl attrsImpl_ Props_AppShellComponentImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
appShellAside = element (unsafeCoerce appShellAsideComponent) <<< toNative

appShellAside_ :: Array JSX -> JSX
appShellAside_ children = appShellAside { children }

foreign import appShellAsideComponent  :: ReactComponent (Record Props_AppShellComponentImpl)

appShellFooter
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_AppShellComponent
  => Union attrsImpl attrsImpl_ Props_AppShellComponentImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
appShellFooter = element (unsafeCoerce appShellFooterComponent) <<< toNative

appShellFooter_ :: Array JSX -> JSX
appShellFooter_ children = appShellFooter { children }

foreign import appShellFooterComponent :: ReactComponent (Record Props_AppShellComponentImpl)

type Props_AppShellComponent =
  Props_Common
    ( children   :: Array JSX
    , withBorder :: Boolean
    , zIndex     :: ZIndex
    )

type Props_AppShellComponentImpl =
  Props_CommonImpl
    ( children   :: Array JSX
    , withBorder :: Boolean
    , zIndex     :: ZIndexImpl
    )
