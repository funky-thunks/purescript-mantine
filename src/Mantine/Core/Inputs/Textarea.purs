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
    , maxRows  :: Maybe Int
    , minRows  :: Maybe Int
    , onChange :: InputHandler
    , value    :: Maybe String
    )

type TextareaPropsImpl =
  InputComponentImpl
    ( autosize :: Boolean
    , maxRows  :: Nullable Number
    , minRows  :: Nullable Number
    , onChange :: InputHandlerImpl
    , value    :: Nullable String
    )
