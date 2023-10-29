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
  ThemingProps
    ( children   :: Array JSX
    , radius     :: Maybe MantineNumberSize
    , shadow     :: Maybe MantineSize
    , withBorder :: Boolean
    )

type PaperPropsImpl =
  ThemingPropsImpl
    ( children   :: Array JSX
    , radius     :: Nullable MantineNumberSizeImpl
    , shadow     :: Nullable String
    , withBorder :: Boolean
    )
