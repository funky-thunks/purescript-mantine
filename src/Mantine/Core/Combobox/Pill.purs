module Mantine.Core.Combobox.Pill
  ( pill
  , pill_
  , Props_Pill
  , Props_PillImpl

  , pillGroup
  , pillGroup_
  , Props_PillGroup
  , Props_PillGroupImpl
  ) where

import Mantine.Core.Prelude
import React.Basic.DOM as DOM

pill
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Pill
  => Union attrsImpl attrsImpl_ Props_PillImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
pill = element (unsafeCoerce pillComponent) <<< toNative

pill_ :: String -> JSX
pill_ t = pill { children: [ DOM.text t ] }

foreign import pillComponent :: ReactComponent (Record Props_PillImpl)

-- Not supported properties
--   { removeButtonProps :: React.ComponentPropsWithoutRef<"button">
--   }

type Props_Pill =
  Props_Common
    ( children         :: Array JSX
    , disabled         :: Boolean
    , key              :: String
    , onRemove         :: Effect Unit
    , radius           :: MantineNumberSize
    , size             :: MantineSize
    , withRemoveButton :: Boolean
    )

type Props_PillImpl =
  Props_CommonImpl
    ( children         :: Array JSX
    , disabled         :: Boolean
    , key              :: String
    , onRemove         :: Effect Unit
    , radius           :: MantineNumberSizeImpl
    , size             :: MantineSizeImpl
    , withRemoveButton :: Boolean
    )

pillGroup
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_PillGroup
  => Union attrsImpl attrsImpl_ Props_PillGroupImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
pillGroup = element (unsafeCoerce pillGroupComponent) <<< toNative

pillGroup_ :: Array JSX -> JSX
pillGroup_ children =  pillGroup { children }

foreign import pillGroupComponent :: ReactComponent (Record Props_PillGroupImpl)

type Props_PillGroup =
  Props_Common
    ( children :: Array JSX
    , disabled :: Boolean
    , gap      :: MantineNumberSize
    , size     :: MantineNumberSize
    )

type Props_PillGroupImpl =
  Props_CommonImpl
    ( children :: Array JSX
    , disabled :: Boolean
    , gap      :: MantineNumberSizeImpl
    , size     :: MantineNumberSizeImpl
    )
