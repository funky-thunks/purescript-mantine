module Mantine.Core.Layout.Flex
  ( flex
  , flex_
  , FlexProps
  , FlexDirection(..)
  , FlexWrap(..)
  ) where

import Mantine.Core.Prelude

flex :: (FlexProps -> FlexProps) -> JSX
flex = mkComponentWithDefault flexComponent defaultThemingProps_

flex_ :: Array JSX -> JSX
flex_ children = flex _ { children = children }

foreign import flexComponent :: ReactComponent FlexPropsImpl

type FlexProps =
  ThemingProps
    ( children  :: Array JSX
    , align     :: Maybe AlignItems
    , columnGap :: Maybe MantineSize
    , direction :: Maybe FlexDirection
    , gap       :: Maybe MantineSize
    , justify   :: Maybe JustifyContent
    , rowGap    :: Maybe MantineSize
    , wrap      :: FlexWrap
    )

data FlexDirection
  = FlexDirectionRow
  | FlexDirectionRowReverse
  | FlexDirectionColumn
  | FlexDirectionColumnReverse
  | FlexDirectionInherit
  | FlexDirectionInitial
  | FlexDirectionRevert
  | FlexDirectionRevertLayer
  | FlexDirectionUnset

instance ToFFI FlexDirection String where
  toNative = case _ of
    FlexDirectionRow           -> "row"
    FlexDirectionRowReverse    -> "row-reverse"
    FlexDirectionColumn        -> "column"
    FlexDirectionColumnReverse -> "column-reverse"
    FlexDirectionInherit       -> "inherit"
    FlexDirectionInitial       -> "initial"
    FlexDirectionRevert        -> "revert"
    FlexDirectionRevertLayer   -> "revert-layer"
    FlexDirectionUnset         -> "unset"

data FlexWrap
  = FlexWrapNowrap
  | FlexWrapWrap
  | FlexWrapWrapReverse
  | FlexWrapInherit
  | FlexWrapInitial
  | FlexWrapRevert
  | FlexWrapRevertLayer
  | FlexWrapUnset

instance ToFFI FlexWrap String where
  toNative = case _ of
    FlexWrapNowrap       -> "nowrap"
    FlexWrapWrap         -> "wrap"
    FlexWrapWrapReverse  -> "wrap-reverse"
    FlexWrapInherit      -> "inherit"
    FlexWrapInitial      -> "initial"
    FlexWrapRevert       -> "revert"
    FlexWrapRevertLayer  -> "revert-layer"
    FlexWrapUnset        -> "unset"

instance DefaultValue FlexWrap where defaultValue = FlexWrapNowrap

type FlexPropsImpl =
  ThemingPropsImpl
    ( children  :: Array JSX
    , align     :: Nullable String
    , columnGap :: Nullable String
    , direction :: Nullable String
    , gap       :: Nullable String
    , justify   :: Nullable String
    , rowGap    :: Nullable String
    , wrap      :: String
    )
