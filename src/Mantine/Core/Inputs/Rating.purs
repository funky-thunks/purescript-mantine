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
    ( color                 :: Maybe MantineColor
    , count                 :: Maybe Number
    , emptySymbol           :: Maybe (Int -> JSX)
    , fractions             :: Maybe Number
    , fullSymbol            :: Maybe (Int -> JSX)
    , getSymbolLabel        :: Maybe (Int -> String)
    , highlightSelectedOnly :: Boolean
    , name                  :: Maybe String
    , onHover               :: ValueHandler Number
    , readOnly              :: Boolean
    , size                  :: Maybe MantineSize
    | Controlled Number
    )

type RatingPropsImpl =
  MantineComponentImpl
    ( color                 :: Nullable MantineColorImpl
    , count                 :: Nullable Number
    , emptySymbol           :: Nullable (Int -> JSX)
    , fractions             :: Nullable Number
    , fullSymbol            :: Nullable (Int -> JSX)
    , getSymbolLabel        :: Nullable (Int -> String)
    , highlightSelectedOnly :: Boolean
    , name                  :: Nullable String
    , onHover               :: ValueHandlerImpl Number
    , readOnly              :: Boolean
    , size                  :: Nullable MantineSizeImpl
    | ControlledImpl Number
    )

ratingToImpl :: RatingProps -> RatingPropsImpl
ratingToImpl props =
  let rest = toNative
         <<< delete (Proxy :: Proxy "emptySymbol")
         <<< delete (Proxy :: Proxy "fullSymbol")
         <<< delete (Proxy :: Proxy "getSymbolLabel")
   in { emptySymbol:    toNullable props.emptySymbol
      , fullSymbol:     toNullable props.fullSymbol
      , getSymbolLabel: toNullable props.getSymbolLabel
      } `union` rest props
