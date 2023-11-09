module Mantine.Core.Miscellaneous.Paper
  ( paper
  , paper_
  , PaperProps
  ) where

import Mantine.Core.Prelude

paper :: (PaperProps -> PaperProps) -> JSX
paper = mkTrivialComponent paperComponent

paper_ :: Array JSX -> JSX
paper_ children = paper _ { children = children }

foreign import paperComponent :: ReactComponent PaperPropsImpl

type PaperProps =
  MantineComponent
    ( children   :: Array JSX
    , radius     :: Maybe MantineNumberSize
    , shadow     :: Maybe MantineShadow
    , withBorder :: Boolean
    )

type PaperPropsImpl =
  MantineComponentImpl
    ( children   :: Array JSX
    , radius     :: Nullable MantineNumberSizeImpl
    , shadow     :: Nullable MantineShadowImpl
    , withBorder :: Boolean
    )
