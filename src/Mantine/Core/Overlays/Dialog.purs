module Mantine.Core.Overlays.Dialog
  ( dialog
  , DialogProps
  , DialogPosition
  ) where

import Mantine.Core.Prelude

dialog :: (DialogProps -> DialogProps) -> JSX
dialog = mkComponentWithDefault dialogComponent defaultDialogProps

foreign import dialogComponent :: ReactComponent DialogPropsImpl

type DialogProps =
  ThemingProps
    ( children                 :: Array JSX
    , onClose                  :: Effect Unit
    , opened                   :: Maybe Boolean
    , position                 :: Maybe DialogPosition
    , radius                   :: Maybe MantineNumberSize
 -- , shadow -- TODO
    , size                     :: Maybe Dimension
    , transition               :: Maybe MantineTransition
    , transitionDuration       :: Maybe Number
    , transitionTimingFunction :: Maybe String
    , withBorder               :: Maybe Boolean
    , withCloseButton          :: Maybe Boolean
    , zIndex                   :: Maybe Number
    )

type DialogPosition =
  { bottom :: Maybe Dimension
  , left   :: Maybe Dimension
  , right  :: Maybe Dimension
  , top    :: Maybe Dimension
  }

defaultDialogProps :: DialogProps
defaultDialogProps = defaultThemingProps { onClose: pure unit }

type DialogPropsImpl =
  ThemingPropsImpl
    ( children                 :: Array JSX
    , onClose                  :: Effect Unit
    , opened                   :: Nullable Boolean
    , position                 :: Nullable DialogPositionImpl
    , radius                   :: Nullable MantineNumberSizeImpl
 -- , shadow -- TODO
    , size                     :: Nullable DimensionImpl
    , transition               :: Nullable String
    , transitionDuration       :: Nullable Number
    , transitionTimingFunction :: Nullable String
    , withBorder               :: Nullable Boolean
    , withCloseButton          :: Nullable Boolean
    , zIndex                   :: Nullable Number
    )

type DialogPositionImpl =
  { bottom :: Nullable DimensionImpl
  , left   :: Nullable DimensionImpl
  , right  :: Nullable DimensionImpl
  , top    :: Nullable DimensionImpl
  }
