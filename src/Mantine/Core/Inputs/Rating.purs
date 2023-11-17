module Mantine.Core.Inputs.Rating
  ( rating
  , Props_Rating
  , Props_RatingImpl
  ) where

import Mantine.Core.Prelude

rating
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Rating
  => Union attrsImpl attrsImpl_ Props_RatingImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
rating = element (unsafeCoerce ratingComponent) <<< toNative

foreign import ratingComponent :: ReactComponent (Record Props_RatingImpl)

type Props_Rating =
  Props_Common
    ( color                 :: MantineColor
    , count                 :: Number
    , emptySymbol           :: Int -> JSX
    , fractions             :: Number
    , fullSymbol            :: Int -> JSX
    , getSymbolLabel        :: Int -> String
    , highlightSelectedOnly :: Boolean
    , name                  :: String
    , onHover               :: ValueHandler Number
    , readOnly              :: Boolean
    , size                  :: MantineSize
    | Controlled Number
    )

type Props_RatingImpl =
  Props_CommonImpl
    ( color                 :: MantineColorImpl
    , count                 :: Number
    , emptySymbol           :: Number -> JSX
    , fractions             :: Number
    , fullSymbol            :: Number -> JSX
    , getSymbolLabel        :: Number -> String
    , highlightSelectedOnly :: Boolean
    , name                  :: String
    , onHover               :: ValueHandlerImpl Number
    , readOnly              :: Boolean
    , size                  :: MantineSizeImpl
    | ControlledImpl Number
    )
