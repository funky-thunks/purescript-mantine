module Mantine.Core.Inputs.NativeSelect
  ( nativeSelect
  , NativeSelectProps
  ) where

import Mantine.Core.Combobox.Select (SelectItem, SelectItemImpl)
import Mantine.Core.Inputs.Input (InputComponent, InputComponentImpl)
import Mantine.Core.Prelude

nativeSelect :: (NativeSelectProps -> NativeSelectProps) -> JSX
nativeSelect = mkTrivialComponent nativeSelectComponent

foreign import nativeSelectComponent :: ReactComponent NativeSelectPropsImpl

type NativeSelectProps =
  InputComponent
    ( data     :: Array SelectItem
    , onChange :: InputHandler
    , value    :: Maybe String
    )

type NativeSelectPropsImpl =
  InputComponentImpl
    ( data     :: Array SelectItemImpl
    , onChange :: InputHandlerImpl
    , value    :: Nullable String
    )
