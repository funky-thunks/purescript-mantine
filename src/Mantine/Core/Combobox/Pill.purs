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
    , key              :: Maybe String
    , onRemove         :: Effect Unit
    , radius           :: Maybe MantineNumberSize
    , size             :: Maybe MantineSize
    , withRemoveButton :: Boolean
    )

defaultPillProps :: PillProps
defaultPillProps = defaultMantineComponent { onRemove: pure unit }

type PillPropsImpl =
  MantineComponentImpl
    ( children         :: Array JSX
    , disabled         :: Boolean
    , key              :: Nullable String
    , onRemove         :: Effect Unit
    , radius           :: Nullable MantineNumberSizeImpl
    , size             :: Nullable MantineSizeImpl
    , withRemoveButton :: Boolean
    )

pillGroup :: (PillGroupProps -> PillGroupProps) -> JSX
pillGroup = mkTrivialComponent pillGroupComponent

foreign import pillGroupComponent :: ReactComponent PillGroupPropsImpl

type PillGroupProps =
  MantineComponent
    ( children :: Array JSX
    , disabled :: Boolean
    , gap      :: Maybe MantineNumberSize
    , size     :: Maybe MantineNumberSize
    )

type PillGroupPropsImpl =
  MantineComponentImpl
    ( children :: Array JSX
    , disabled :: Boolean
    , gap      :: Nullable MantineNumberSizeImpl
    , size     :: Nullable MantineNumberSizeImpl
    )
