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
  ThemingProps
    ( children          :: Array JSX
    , disabled          :: Boolean
    , key               :: Maybe String
    , onRemove          :: Effect Unit
    , radius            :: Maybe MantineNumberSize
    , size              :: Maybe MantineSize
    , withRemoveButton  :: Boolean
    )

defaultPillProps :: PillProps
defaultPillProps = defaultThemingProps { onRemove: pure unit }

type PillPropsImpl =
  ThemingPropsImpl
    ( children          :: Array JSX
    , disabled          :: Boolean
    , key               :: Nullable String
    , onRemove          :: Effect Unit
    , radius            :: Nullable MantineNumberSizeImpl
    , size              :: Nullable String
    , withRemoveButton  :: Boolean
    )

pillGroup :: (PillGroupProps -> PillGroupProps) -> JSX
pillGroup = mkTrivialComponent pillGroupComponent

foreign import pillGroupComponent :: ReactComponent PillGroupPropsImpl

type PillGroupProps =
  ThemingProps
    ( children :: Array JSX
    , disabled :: Boolean
    , gap      :: Maybe MantineNumberSize
    , size     :: Maybe MantineNumberSize
    )

type PillGroupPropsImpl =
  ThemingPropsImpl
    ( children :: Array JSX
    , disabled :: Boolean
    , gap      :: Nullable MantineNumberSizeImpl
    , size     :: Nullable MantineNumberSizeImpl
    )
