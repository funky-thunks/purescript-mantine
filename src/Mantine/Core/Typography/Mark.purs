module Mantine.Core.Typography.Mark
  ( mark
  , Props_Mark
  , Props_MarkImpl
  ) where

import Mantine.Core.Prelude

mark
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Mark
  => Union attrsImpl attrsImpl_ Props_MarkImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
mark = element (unsafeCoerce markComponent) <<< toNative

foreign import markComponent :: ReactComponent (Record Props_MarkImpl)

type Props_Mark =
  Props_Common
    ( children :: String
    , color    :: MantineColor
    )

type Props_MarkImpl =
  Props_CommonImpl
    ( children :: String
    , color    :: MantineColorImpl
    )
