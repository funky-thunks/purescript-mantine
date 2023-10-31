module Mantine.Core.Layout.AppShell
  ( appShell
  , AppShellProps
  , AppShellCollapse
  , AppShellHorizontalConfiguration
  , AppShellLayout(..)
  , AppShellPadding(..)
  , AppShellResponsiveSize
  , AppShellRules(..)
  , AppShellSize(..)
  , AppShellVerticalConfiguration

  , appShellMain
  , AppShellMainProps

  , appShellSection
  , appShellSection_
  , AppShellSectionProps

  -- AppShell Components
  , AppShellComponentProps
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

appShell :: (AppShellProps -> AppShellProps) -> JSX
appShell = mkTrivialComponent appShellComponent

foreign import appShellComponent :: ReactComponent AppShellPropsImpl

type AppShellProps =
  ThemingProps
    ( aside                    :: Maybe AppShellVerticalConfiguration
    , disabled                 :: Maybe Boolean
    , footer                   :: Maybe AppShellHorizontalConfiguration
    , header                   :: Maybe AppShellHorizontalConfiguration
    , layout                   :: Maybe AppShellLayout
    , navbar                   :: Maybe AppShellVerticalConfiguration
    , padding                  :: Maybe AppShellPadding
    , transitionDuration       :: Maybe Milliseconds
    , transitionTimingFunction :: Maybe MantineTransitionTimingFunction
    , withBorder               :: Boolean
    , zIndex                   :: Maybe Number
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
  , breakpoint :: Maybe MantineNumberSize
  , collapsed  :: Maybe AppShellCollapse
  }

type AppShellCollapse = { desktop :: Boolean, mobile :: Boolean }

type AppShellPropsImpl =
  ThemingPropsImpl
    ( aside                    :: Nullable AppShellVerticalConfigurationImpl
    , disabled                 :: Nullable Boolean
    , footer                   :: Nullable AppShellHorizontalConfigurationImpl
    , header                   :: Nullable AppShellHorizontalConfigurationImpl
    , layout                   :: Nullable String
    , navbar                   :: Nullable AppShellVerticalConfigurationImpl
    , padding                  :: Nullable AppShellPaddingImpl
    , transitionDuration       :: Nullable Number
    , transitionTimingFunction :: Nullable String
    , withBorder               :: Boolean
    , zIndex                   :: Nullable Number
    )

type AppShellHorizontalConfigurationImpl =
  { height    :: AppShellSizeImpl
  , collapsed :: Boolean
  , offset    :: Boolean
  }

type AppShellVerticalConfigurationImpl =
  { width      :: AppShellSizeImpl
  , breakpoint :: Nullable MantineNumberSizeImpl
  , collapsed  :: Nullable AppShellCollapse
  }

newtype AppShellRules = AppShellRules (Array (String /\ Either String Number))

instance DefaultValue AppShellRules where defaultValue = AppShellRules []

type AppShellRulesImpl = Object (String |+| Number)

instance ToFFI AppShellRules AppShellRulesImpl where
  toNative (AppShellRules rs) =
    let f = either asOneOf asOneOf
     in fromFoldable (map f <$> rs)

appShellSection :: (AppShellSectionProps -> AppShellSectionProps) -> JSX
appShellSection = mkTrivialComponent appShellSectionComponent

appShellSection_ :: Array JSX -> JSX
appShellSection_ children = appShellSection _ { children = children }

foreign import appShellSectionComponent :: ReactComponent AppShellSectionPropsImpl

type AppShellSectionProps =
  ThemingProps
    ( children :: Array JSX
    , grow     :: Boolean
    )

type AppShellSectionPropsImpl =
  ThemingPropsImpl
    ( children :: Array JSX
    , grow     :: Boolean
    )

appShellMain :: (AppShellMainProps -> AppShellMainProps) -> JSX
appShellMain = mkTrivialComponent appShellMainComponent

foreign import appShellMainComponent :: ReactComponent AppShellMainPropsImpl

type AppShellMainProps = ThemingProps (children :: Array JSX)

type AppShellMainPropsImpl = ThemingPropsImpl (children :: Array JSX)

appShellNavbar :: (AppShellComponentProps -> AppShellComponentProps) -> JSX
appShellNavbar = mkTrivialComponent appShellNavbarComponent

appShellNavbar_ :: Array JSX -> JSX
appShellNavbar_ children = appShellNavbar _ { children = children }

foreign import appShellNavbarComponent :: ReactComponent AppShellComponentPropsImpl

appShellHeader :: (AppShellComponentProps -> AppShellComponentProps) -> JSX
appShellHeader = mkTrivialComponent appShellHeaderComponent

appShellHeader_ :: Array JSX -> JSX
appShellHeader_ children = appShellHeader _ { children = children }

foreign import appShellHeaderComponent :: ReactComponent AppShellComponentPropsImpl

appShellAside :: (AppShellComponentProps -> AppShellComponentProps) -> JSX
appShellAside = mkTrivialComponent appShellAsideComponent

appShellAside_ :: Array JSX -> JSX
appShellAside_ children = appShellAside _ { children = children }

foreign import appShellAsideComponent  :: ReactComponent AppShellComponentPropsImpl

appShellFooter :: (AppShellComponentProps -> AppShellComponentProps) -> JSX
appShellFooter = mkTrivialComponent appShellFooterComponent

appShellFooter_ :: Array JSX -> JSX
appShellFooter_ children = appShellFooter _ { children = children }

foreign import appShellFooterComponent :: ReactComponent AppShellComponentPropsImpl

type AppShellComponentProps =
  ThemingProps
    ( boolean  :: Boolean
    , children :: Array JSX
    , zIndex   :: Maybe Number
    )

type AppShellComponentPropsImpl =
  ThemingPropsImpl
    ( boolean  :: Boolean
    , children :: Array JSX
    , zIndex   :: Nullable Number
    )
