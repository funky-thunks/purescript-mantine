module Mantine.Core.Miscellaneous.Paper
  ( paper
  , paper_
  , PaperProps
  ) where

import Prelude
import Mantine.Core.Prelude

paper :: (PaperProps -> PaperProps) -> JSX
paper = mkComponentWithDefault paperComponent defaultPaperProps

paper_ :: Array JSX -> JSX
paper_ children = paper _ { children = children }

foreign import paperComponent :: ReactComponent PaperPropsImpl

type PaperProps =
  ThemingProps
    ( children   :: Array JSX
    , radius     :: MantineNumberSize
    , shadow     :: Maybe MantineSize
    , withBorder :: Boolean
    )

defaultPaperProps :: PaperProps
defaultPaperProps =
  defaultThemingProps
    { radius: Preset Small
    } `union` defaultValue

type PaperPropsImpl =
  ThemingPropsImpl
    ( children   :: Array JSX
    , radius     :: MantineNumberSizeImpl
    , shadow     :: Nullable String
    , withBorder :: Boolean
    )
