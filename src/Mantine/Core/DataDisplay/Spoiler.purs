module Mantine.Core.DataDisplay.Spoiler
  ( spoiler
  , spoiler_
  , SpoilerProps
  , SpoilerState(..)
  ) where

import Mantine.Core.Prelude
import React.Basic.DOM as DOM

spoiler :: (SpoilerProps -> SpoilerProps) -> JSX
spoiler = mkComponentWithDefault spoilerComponent defaultSpoilerProps

spoiler_ :: Array JSX -> JSX
spoiler_ children = spoiler _ { children = children }

foreign import spoilerComponent :: ReactComponent SpoilerPropsImpl

type SpoilerProps =
  ThemingProps
    ( children           :: Array JSX
    , hideLabel          :: JSX
    , initialState       :: SpoilerState
    , maxHeight          :: Number
    , showLabel          :: JSX
    , transitionDuration :: Maybe Number
    )

defaultSpoilerProps :: SpoilerProps
defaultSpoilerProps =
  defaultThemingProps
    { hideLabel:    DOM.text "hide"
    , initialState: SpoilerFolded
    , maxHeight:    120.0
    , showLabel:    DOM.text "show"
    }

data SpoilerState
  = SpoilerFolded
  | SpoilerUnfolded

instance ToFFI SpoilerState Boolean where
  toNative = case _ of
    SpoilerFolded   -> false
    SpoilerUnfolded -> true

type SpoilerPropsImpl =
  ThemingPropsImpl
    ( children           :: Array JSX
    , hideLabel          :: JSX
    , initialState       :: Boolean
    , maxHeight          :: Number
    , showLabel          :: JSX
    , transitionDuration :: Nullable Number
    )
