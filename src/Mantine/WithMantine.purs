module Mantine.WithMantine
  ( withMantine
  ) where

import React.Basic (JSX, ReactComponent, element)

withMantine :: JSX -> JSX
withMantine children = element mantineProviderComponent { children: [ children ] }

foreign import mantineProviderComponent :: ReactComponent { children :: Array JSX }
