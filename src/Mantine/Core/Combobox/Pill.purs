module Mantine.Core.Combobox.Pill
  ( pill
  , PillProps

  , pillGroup
  , PillGroupProps
  ) where

import Mantine.Core.Prelude

pill :: (PillProps -> PillProps) -> JSX
pill = mkComponentWithDefault pillComponent defaultPillProps

foreign import pillComponent :: ReactComponent PillPropsImpl

-- Not supported properties
--   { removeButtonProps :: React.ComponentPropsWithoutRef<"button">
--   }

type PillProps =
  MantineComponent
    ( children         :: Array JSX
    , disabled         :: Boolean
    , key              :: Optional String
    , onRemove         :: Effect Unit
    , radius           :: Optional MantineNumberSize
    , size             :: Optional MantineSize
    , withRemoveButton :: Boolean
    )

defaultPillProps :: PillProps
defaultPillProps = defaultMantineComponent { onRemove: pure unit }

type PillPropsImpl =
  MantineComponentImpl
    ( children         :: Array JSX
    , disabled         :: Boolean
    , key              :: OptionalImpl String
    , onRemove         :: Effect Unit
    , radius           :: OptionalImpl MantineNumberSizeImpl
    , size             :: OptionalImpl MantineSizeImpl
    , withRemoveButton :: Boolean
    )

pillGroup :: (PillGroupProps -> PillGroupProps) -> JSX
pillGroup = mkTrivialComponent pillGroupComponent

foreign import pillGroupComponent :: ReactComponent PillGroupPropsImpl

type PillGroupProps =
  MantineComponent
    ( children :: Array JSX
    , disabled :: Boolean
    , gap      :: Optional MantineNumberSize
    , size     :: Optional MantineNumberSize
    )

type PillGroupPropsImpl =
  MantineComponentImpl
    ( children :: Array JSX
    , disabled :: Boolean
    , gap      :: OptionalImpl MantineNumberSizeImpl
    , size     :: OptionalImpl MantineNumberSizeImpl
    )
