module Mantine.Core.Inputs.ClearButtonProps
  ( ClearButtonProps
  , ClearButtonPropsImpl
  ) where

import Mantine.Core.Prelude

type ClearButtonProps =
  { "aria-label" :: Optional String
  , tabIndex     :: Optional Int
  }

type ClearButtonPropsImpl =
  { "aria-label" :: OptionalImpl String
  , tabIndex     :: OptionalImpl Number
  }
