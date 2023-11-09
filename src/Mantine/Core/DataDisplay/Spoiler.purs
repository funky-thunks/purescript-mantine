module Mantine.Core.DataDisplay.Spoiler
  ( spoiler
  , spoiler_
  , SpoilerProps
  , SpoilerState(..)
  ) where

import Mantine.Core.Prelude
import React.Basic.DOM as DOM
import Web.HTML.HTMLButtonElement (HTMLButtonElement)

spoiler :: (SpoilerProps -> SpoilerProps) -> JSX
spoiler = mkComponentWithDefault spoilerComponent defaultSpoilerProps

spoiler_ :: Array JSX -> JSX
spoiler_ children = spoiler _ { children = children }

foreign import spoilerComponent :: ReactComponent SpoilerPropsImpl

type SpoilerProps =
  MantineComponent
    ( children           :: Array JSX
    , controlRef         :: Maybe (Ref HTMLButtonElement)
    , hideLabel          :: JSX
    , initialState       :: SpoilerState
    , maxHeight          :: Pixels
    , showLabel          :: JSX
    , transitionDuration :: Maybe Milliseconds
    )

defaultSpoilerProps :: SpoilerProps
defaultSpoilerProps =
  defaultMantineComponent
    { hideLabel:    DOM.text "hide"
    , initialState: SpoilerFolded
    , maxHeight:    120.0
    , showLabel:    DOM.text "show"
    }

data SpoilerState
  = SpoilerFolded
  | SpoilerUnfolded

type SpoilerStateImpl = Boolean

instance ToFFI SpoilerState SpoilerStateImpl where
  toNative = case _ of
    SpoilerFolded   -> false
    SpoilerUnfolded -> true

type SpoilerPropsImpl =
  MantineComponentImpl
    ( children           :: Array JSX
    , controlRef         :: Nullable (Ref HTMLButtonElement)
    , hideLabel          :: JSX
    , initialState       :: SpoilerStateImpl
    , maxHeight          :: PixelsImpl
    , showLabel          :: JSX
    , transitionDuration :: Nullable MillisecondsImpl
    )
