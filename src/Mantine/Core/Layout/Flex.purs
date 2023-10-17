module Mantine.Core.Layout.Flex
  ( flex
  , flex_
  , FlexProps
  , FlexDirection(..)
  , FlexWrap(..)

  , module Mantine.Core.Common
  ) where

import Data.Default (class DefaultValue)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Mantine.Core.Common (AlignItems(..), JustifyContent(..), MantineNumberSize, MantineSize(..))
import Mantine.Core.Common as MC
import Mantine.FFI (class ToFFI)
import React.Basic (ReactComponent)
import React.Basic.Hooks (JSX)

flex :: (FlexProps -> FlexProps) -> JSX
flex = MC.mkTrivialComponent flexComponent MC.defaultThemingProps_

flex_ :: Array JSX -> JSX
flex_ children = flex _ { children = children }

foreign import flexComponent :: ReactComponent FlexPropsImpl

type FlexProps =
  MC.ThemingProps
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
  MC.ThemingPropsImpl
    ( children  :: Array JSX
    , align     :: Nullable String
    , columnGap :: Nullable String
    , direction :: Nullable String
    , gap       :: Nullable String
    , justify   :: Nullable String
    , rowGap    :: Nullable String
    , wrap      :: String
    )
