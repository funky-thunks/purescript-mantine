module Mantine.Core.Miscellaneous.Paper
  ( paper
  , paper_
  , Props_Paper
  , Props_PaperImpl
  ) where

import Mantine.Core.Prelude

paper
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Paper
  => Union attrsImpl attrsImpl_ Props_PaperImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
paper = element (unsafeCoerce paperComponent) <<< toNative

paper_ :: Array JSX -> JSX
paper_ children = paper { children }

foreign import paperComponent :: ReactComponent (Record Props_PaperImpl)

type Props_Paper =
  Props_Common
    ( children   :: Array JSX
    , radius     :: MantineNumberSize
    , shadow     :: MantineShadow
    , withBorder :: Boolean
    )

type Props_PaperImpl =
  Props_CommonImpl
    ( children   :: Array JSX
    , radius     :: MantineNumberSizeImpl
    , shadow     :: MantineShadowImpl
    , withBorder :: Boolean
    )
