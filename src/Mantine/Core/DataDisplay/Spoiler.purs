module Mantine.Core.DataDisplay.Spoiler
  ( spoiler
  , spoiler_
  , SpoilerProps
  , SpoilerState(..)
  ) where

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Mantine.Core.Common as MC
import React.Basic (ReactComponent, element)
import React.Basic.DOM as DOM
import React.Basic.Hooks (JSX)

spoiler :: (SpoilerProps -> SpoilerProps) -> JSX
spoiler setProps = element spoilerComponent (spoilerToImpl (setProps defaultSpoilerProps))

spoiler_ :: Array JSX -> JSX
spoiler_ children = spoiler _ { children = children }

foreign import spoilerComponent :: ReactComponent SpoilerPropsImpl

type SpoilerProps =
  MC.ThemingProps
    ( children           :: Array JSX
    , hideLabel          :: JSX
    , initialState       :: SpoilerState
    , maxHeight          :: Number
    , showLabel          :: JSX
    , transitionDuration :: Maybe Number
    )

defaultSpoilerProps :: SpoilerProps
defaultSpoilerProps =
  MC.defaultThemingProps
    { children:           []
    , hideLabel:          DOM.text "hide"
    , initialState:       SpoilerFolded
    , maxHeight:          120.0
    , showLabel:          DOM.text "show"
    , transitionDuration: Nothing
    }

data SpoilerState
  = SpoilerFolded
  | SpoilerUnfolded

stateNative :: SpoilerState -> Boolean
stateNative = case _ of
  SpoilerFolded   -> false
  SpoilerUnfolded -> true

type SpoilerPropsImpl =
  MC.ThemingPropsImpl
    ( children           :: Array JSX
    , hideLabel          :: JSX
    , initialState       :: Boolean
    , maxHeight          :: Number
    , showLabel          :: JSX
    , transitionDuration :: Nullable Number
    )

spoilerToImpl :: SpoilerProps -> SpoilerPropsImpl
spoilerToImpl =
  MC.themingToImpl \ props@{ children, hideLabel, maxHeight, showLabel } ->
    { children, hideLabel, maxHeight, showLabel
    , initialState: stateNative props.initialState
    , transitionDuration: toNullable props.transitionDuration
    }
