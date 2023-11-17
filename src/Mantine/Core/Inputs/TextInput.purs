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
    , value    :: Optional String
    )

type TextInputPropsImpl =
  InputComponentImpl
    ( onChange :: InputHandlerImpl
    , value    :: OptionalImpl String
    )
