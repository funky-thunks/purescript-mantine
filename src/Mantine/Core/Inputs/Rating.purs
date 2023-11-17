module Mantine.Core.Inputs.Rating
  ( rating
  , RatingProps
  ) where

import Mantine.Core.Prelude

rating :: (RatingProps -> RatingProps) -> JSX
rating = mkComponent ratingComponent ratingToImpl defaultMantineComponent_

foreign import ratingComponent :: ReactComponent RatingPropsImpl

type RatingProps =
  MantineComponent
    ( color                 :: Optional MantineColor
    , count                 :: Optional Number
    , emptySymbol           :: Optional (Int -> JSX)
    , fractions             :: Optional Number
    , fullSymbol            :: Optional (Int -> JSX)
    , getSymbolLabel        :: Optional (Int -> String)
    , highlightSelectedOnly :: Boolean
    , name                  :: Optional String
    , onHover               :: ValueHandler Number
    , readOnly              :: Boolean
    , size                  :: Optional MantineSize
    | Controlled Number
    )

type RatingPropsImpl =
  MantineComponentImpl
    ( color                 :: OptionalImpl MantineColorImpl
    , count                 :: OptionalImpl Number
    , emptySymbol           :: OptionalImpl (Int -> JSX)
    , fractions             :: OptionalImpl Number
    , fullSymbol            :: OptionalImpl (Int -> JSX)
    , getSymbolLabel        :: OptionalImpl (Int -> String)
    , highlightSelectedOnly :: Boolean
    , name                  :: OptionalImpl String
    , onHover               :: ValueHandlerImpl Number
    , readOnly              :: Boolean
    , size                  :: OptionalImpl MantineSizeImpl
    | ControlledImpl Number
    )

ratingToImpl :: RatingProps -> RatingPropsImpl
ratingToImpl props =
  let rest = toNative
         <<< delete (Proxy :: Proxy "emptySymbol")
         <<< delete (Proxy :: Proxy "fullSymbol")
         <<< delete (Proxy :: Proxy "getSymbolLabel")
   in { emptySymbol:    toOptionalImpl props.emptySymbol
      , fullSymbol:     toOptionalImpl props.fullSymbol
      , getSymbolLabel: toOptionalImpl props.getSymbolLabel
      } `union` rest props
