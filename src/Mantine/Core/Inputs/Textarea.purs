module Mantine.Core.Inputs.Textarea
  ( textarea
  , Props_Textarea
  , Props_TextareaImpl
  ) where

import Mantine.Core.Inputs.Input (Props_InputComponent, Props_InputComponentImpl)
import Mantine.Core.Prelude

textarea
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Textarea
  => Union attrsImpl attrsImpl_ Props_TextareaImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
textarea = element (unsafeCoerce textareaComponent) <<< toNative

foreign import textareaComponent :: ReactComponent (Record Props_TextareaImpl)

type Props_Textarea =
  Props_InputComponent
    ( autosize :: Boolean
    , maxRows  :: Int
    , minRows  :: Int
    , onChange :: InputHandler
    , value    :: String
    )

type Props_TextareaImpl =
  Props_InputComponentImpl
    ( autosize :: Boolean
    , maxRows  :: Number
    , minRows  :: Number
    , onChange :: InputHandlerImpl
    , value    :: String
    )
