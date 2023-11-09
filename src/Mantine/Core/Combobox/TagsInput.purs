module Mantine.Core.Combobox.TagsInput
  ( tagsInput
  , TagsInputProps
  ) where

import Data.Maybe (fromMaybe)
import Effect.Uncurried (mkEffectFn1)
import Mantine.Core.Combobox.Select (BaseSelectPropsRow, BaseSelectPropsRowImpl, baseSelectToImpl)
import Mantine.Core.Prelude

tagsInput :: (TagsInputProps -> TagsInputProps) -> JSX
tagsInput = mkComponent tagsInputComponent tagsInputPropsToImpl defaultMantineComponent_

foreign import tagsInputComponent :: ReactComponent TagsInputPropsImpl

type TagsInputProps =
  MantineComponent
    ( allowDuplicates :: Boolean
    , maxTags         :: Maybe Int
    , onDuplicate     :: ValueHandler String
    , splitChars      :: Maybe (Array String)
    | BaseSelectPropsRow (Array String)
    )

type TagsInputPropsImpl =
  MantineComponentImpl
    ( allowDuplicates :: Boolean
    , maxTags         :: Nullable Number
    , onDuplicate     :: ValueHandlerImpl String
    , splitChars      :: Nullable (Array String)
    | BaseSelectPropsRowImpl (Array String)
    )

tagsInputPropsToImpl :: TagsInputProps -> TagsInputPropsImpl
tagsInputPropsToImpl = baseSelectToImpl (\h -> mkEffectFn1 (h <<< fromMaybe [] <<< toMaybe))
