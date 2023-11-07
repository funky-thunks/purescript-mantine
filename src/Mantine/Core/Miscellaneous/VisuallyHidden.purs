module Mantine.Core.Miscellaneous.VisuallyHidden
  ( visuallyHidden_
  ) where

import Mantine.Core.Prelude

visuallyHidden :: (VisuallyHiddenProps -> VisuallyHiddenProps) -> JSX
visuallyHidden = mkComponent visuallyHiddenComponent toNative defaultValue

visuallyHidden_ :: Array JSX -> JSX
visuallyHidden_ children = visuallyHidden _ { children = children }

type VisuallyHiddenProps = { children :: Array JSX }

foreign import visuallyHiddenComponent :: ReactComponent VisuallyHiddenProps
