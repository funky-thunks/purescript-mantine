module Mantine.Core.Inputs.TextInput
  ( textInput
  , Props_TextInput
  , Props_TextInputImpl
  ) where

import Mantine.Core.Inputs.Input (Props_InputComponent, Props_InputComponentImpl)
import Mantine.Core.Prelude

textInput
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_TextInput
  => Union attrsImpl attrsImpl_ Props_TextInputImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
textInput = element (unsafeCoerce textInputComponent) <<< toNative

foreign import textInputComponent :: ReactComponent (Record Props_TextInputImpl)

type Props_TextInput =
  Props_InputComponent
    ( onChange :: InputHandler
    , value    :: String
    )

type Props_TextInputImpl =
  Props_InputComponentImpl
    ( onChange :: InputHandlerImpl
    , value    :: String
    )
