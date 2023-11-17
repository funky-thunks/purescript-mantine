module Mantine.Core.DataDisplay.Spoiler
  ( spoiler
  , spoiler_
  , Props_Spoiler
  , Props_SpoilerImpl
  , SpoilerState(..)
  , SpoilerStateImpl
  ) where

import Mantine.Core.Prelude
import Web.HTML.HTMLButtonElement (HTMLButtonElement)

spoiler
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Spoiler
  => Union attrsImpl attrsImpl_ Props_SpoilerImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
spoiler = element (unsafeCoerce spoilerComponent) <<< toNative

spoiler_ :: Array JSX -> JSX
spoiler_ children = spoiler { children }

foreign import spoilerComponent :: ReactComponent (Record Props_SpoilerImpl)

type Props_Spoiler =
  Props_Common
    ( children           :: Array JSX
    , controlRef         :: Ref HTMLButtonElement
    , hideLabel          :: JSX
    , initialState       :: SpoilerState
    , maxHeight          :: Pixels
    , showLabel          :: JSX
    , transitionDuration :: Milliseconds
    )

data SpoilerState
  = SpoilerFolded
  | SpoilerUnfolded

type SpoilerStateImpl = Boolean

instance ToFFI SpoilerState SpoilerStateImpl where
  toNative = case _ of
    SpoilerFolded   -> false
    SpoilerUnfolded -> true

type Props_SpoilerImpl =
  Props_CommonImpl
    ( children           :: Array JSX
    , controlRef         :: Ref HTMLButtonElement
    , hideLabel          :: JSX
    , initialState       :: SpoilerStateImpl
    , maxHeight          :: PixelsImpl
    , showLabel          :: JSX
    , transitionDuration :: MillisecondsImpl
    )
