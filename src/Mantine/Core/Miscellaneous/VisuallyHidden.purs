module Mantine.Core.Miscellaneous.VisuallyHidden
  ( visuallyHidden_
  ) where

import Mantine.Core.Prelude

visuallyHidden
  :: forall attrs attrs_
   . Union attrs attrs_ Props_VisuallyHidden
  => Record attrs -> JSX
visuallyHidden = element (unsafeCoerce visuallyHiddenComponent)

visuallyHidden_ :: Array JSX -> JSX
visuallyHidden_ children = visuallyHidden { children }

type Props_VisuallyHidden = ( children :: Array JSX )

foreign import visuallyHiddenComponent :: ReactComponent (Record Props_VisuallyHidden)
