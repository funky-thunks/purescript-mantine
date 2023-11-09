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
    , defaultValue          :: Maybe Number
    , emptySymbol           :: Maybe (Int -> JSX)
    , fractions             :: Maybe Number
    , fullSymbol            :: Maybe (Int -> JSX)
    , getSymbolLabel        :: Maybe (Int -> String)
    , highlightSelectedOnly :: Boolean
    , name                  :: Maybe String
    , onChange              :: ValueHandler Number
    , onHover               :: ValueHandler Number
    , readOnly              :: Boolean
    , size                  :: Maybe MantineSize
    , value                 :: Maybe Number
    )

type RatingPropsImpl =
  MantineComponentImpl
    ( color                 :: Nullable MantineColorImpl
    , count                 :: Nullable Number
    , defaultValue          :: Nullable Number
    , emptySymbol           :: Nullable (Int -> JSX)
    , fractions             :: Nullable Number
    , fullSymbol            :: Nullable (Int -> JSX)
    , getSymbolLabel        :: Nullable (Int -> String)
    , highlightSelectedOnly :: Boolean
    , name                  :: Nullable String
    , onChange              :: ValueHandlerImpl Number
    , onHover               :: ValueHandlerImpl Number
    , readOnly              :: Boolean
    , size                  :: Nullable MantineSizeImpl
    , value                 :: Nullable Number
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
