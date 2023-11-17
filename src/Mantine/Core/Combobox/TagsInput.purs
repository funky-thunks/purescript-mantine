module Mantine.Core.Combobox.TagsInput
  ( tagsInput
  , Props_TagsInput
  , Props_TagsInputImpl
  ) where

import Mantine.Core.Combobox.Select (BaseSelectPropsRow, BaseSelectPropsRowImpl)
import Mantine.Core.Prelude

tagsInput
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_TagsInput
  => Union attrsImpl attrsImpl_ Props_TagsInputImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
tagsInput = element (unsafeCoerce tagsInputComponent) <<< toNative

foreign import tagsInputComponent :: ReactComponent (Record Props_TagsInputImpl)

type Props_TagsInput =
  Props_Common
    ( allowDuplicates :: Boolean
    , maxTags         :: Maybe Int
    , onDuplicate     :: ValueHandler String
    , splitChars      :: Maybe (Array String)
    | BaseSelectPropsRow (Array String)
    )

type Props_TagsInputImpl =
  Props_CommonImpl
    ( allowDuplicates :: Boolean
    , maxTags         :: Nullable Number
    , onDuplicate     :: ValueHandlerImpl String
    , splitChars      :: Nullable (Array String)
    | BaseSelectPropsRowImpl (Array String)
    )
