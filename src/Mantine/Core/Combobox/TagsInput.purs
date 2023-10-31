module Mantine.Core.Combobox.TagsInput
  ( tagsInput
  , TagsInputProps
  ) where

import Data.Maybe (fromMaybe)
import Effect.Uncurried (mkEffectFn1)
import Mantine.Core.Combobox.Select (BaseSelectPropsRow, BaseSelectPropsRowImpl, baseSelectToImpl)
import Mantine.Core.Prelude

tagsInput :: (TagsInputProps -> TagsInputProps) -> JSX
tagsInput = mkComponent tagsInputComponent tagsInputPropsToImpl defaultThemingProps_

foreign import tagsInputComponent :: ReactComponent TagsInputPropsImpl

type TagsInputProps =
  ThemingProps
    ( allowDuplicates :: Boolean
    , maxTags         :: Maybe Int
    , onDuplicate     :: ValueHandler String
    , splitChars      :: Maybe (Array String)
    | BaseSelectPropsRow (Array String)
    )

type TagsInputPropsImpl =
  ThemingPropsImpl
    ( allowDuplicates :: Boolean
    , maxTags         :: Nullable Number
    , onDuplicate     :: EffectFn1 String Unit
    , splitChars      :: Nullable (Array String)
    | BaseSelectPropsRowImpl (Array String)
    )

tagsInputPropsToImpl :: TagsInputProps -> TagsInputPropsImpl
tagsInputPropsToImpl = baseSelectToImpl (\h -> mkEffectFn1 (h <<< fromMaybe [] <<< toMaybe))
