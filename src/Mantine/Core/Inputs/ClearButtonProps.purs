module Mantine.Core.Inputs.ClearButtonProps
  ( ClearButtonProps
  , ClearButtonPropsImpl
  ) where

import Mantine.Core.Prelude

type ClearButtonProps =
  { "aria-label" :: Maybe String
  , tabIndex     :: Maybe Int
  }

type ClearButtonPropsImpl =
  { "aria-label" :: Nullable String
  , tabIndex     :: Nullable Number
  }
