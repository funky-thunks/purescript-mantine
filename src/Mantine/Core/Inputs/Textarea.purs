module Mantine.Core.Inputs.Textarea
  ( textarea
  , TextareaProps
  ) where

import Mantine.Core.Inputs.Input (InputComponent, InputComponentImpl)
import Mantine.Core.Prelude

textarea :: (TextareaProps -> TextareaProps) -> JSX
textarea = mkTrivialComponent textareaComponent

foreign import textareaComponent :: ReactComponent TextareaPropsImpl

type TextareaProps =
  InputComponent
    ( autosize :: Boolean
    , maxRows  :: Optional Int
    , minRows  :: Optional Int
    , onChange :: InputHandler
    , value    :: Optional String
    )

type TextareaPropsImpl =
  InputComponentImpl
    ( autosize :: Boolean
    , maxRows  :: OptionalImpl Number
    , minRows  :: OptionalImpl Number
    , onChange :: InputHandlerImpl
    , value    :: OptionalImpl String
    )
