module Mantine.Core.Inputs.TextInput
  ( textInput
  , TextInputProps
  ) where

import Mantine.Core.Inputs.Input (InputComponent, InputComponentImpl)
import Mantine.Core.Prelude

textInput :: (TextInputProps -> TextInputProps) -> JSX
textInput = mkTrivialComponent textInputComponent

foreign import textInputComponent :: ReactComponent TextInputPropsImpl

type TextInputProps =
  InputComponent
    ( onChange :: InputHandler
    , value    :: Maybe String
    )

type TextInputPropsImpl =
  InputComponentImpl
    ( onChange :: EffectFn1 SyntheticEvent Unit
    , value    :: Nullable String
    )
