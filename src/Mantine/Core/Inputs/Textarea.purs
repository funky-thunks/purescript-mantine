module Mantine.Core.Inputs.Textarea
  ( textarea
  , TextareaProps

  , module Mantine.Core.Inputs.Input
  ) where

import Mantine.Core.Prelude
import Mantine.Core.Inputs.Input (InputVariant(..))

textarea :: (TextareaProps -> TextareaProps) -> JSX
textarea = mkTrivialComponent textareaComponent

foreign import textareaComponent :: ReactComponent TextareaPropsImpl

type TextareaProps =
  ThemingProps
    ( autosize     :: Boolean
    , description  :: Maybe JSX
    , disabled     :: Boolean
    , error        :: Maybe JSX
    , icon         :: Maybe JSX
    , iconWidth    :: Maybe Pixels
    , id           :: Maybe String
    , label        :: Maybe JSX
    , maxRows      :: Maybe Int
    , minRows      :: Maybe Int
    , onChange     :: InputHandler
    , placeholder  :: Maybe String
    , radius       :: Maybe MantineNumberSize
    , required     :: Boolean
    , rightSection :: Maybe JSX
    , size         :: Maybe MantineNumberSize
    , value        :: Maybe String
    , variant      :: InputVariant
    , withAsterisk :: Boolean
    )

type TextareaPropsImpl =
  ThemingPropsImpl
    ( autosize     :: Boolean
    , description  :: Nullable JSX
    , disabled     :: Boolean
    , error        :: Nullable JSX
    , icon         :: Nullable JSX
    , iconWidth    :: Nullable Number
    , id           :: Nullable String
    , label        :: Nullable JSX
    , maxRows      :: Nullable Number
    , minRows      :: Nullable Number
    , onChange     :: EffectFn1 SyntheticEvent Unit
    , placeholder  :: Nullable String
    , radius       :: Nullable MantineNumberSizeImpl
    , required     :: Boolean
    , rightSection :: Nullable JSX
    , size         :: Nullable MantineNumberSizeImpl
    , value        :: Nullable String
    , variant      :: String
    , withAsterisk :: Boolean
    )