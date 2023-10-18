module Mantine.Core.Typography.TypographyStylesProvider
  ( typographyStylesProvider_
  ) where

import React.Basic (ReactComponent, element)
import React.Basic.Hooks (JSX)

typographyStylesProvider_ :: Array JSX -> JSX
typographyStylesProvider_ children = element typographyStylesProviderComponent { children }

foreign import typographyStylesProviderComponent :: ReactComponent { children :: Array JSX }
