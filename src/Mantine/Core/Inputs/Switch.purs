module Mantine.Core.Inputs.Switch
  ( switch
  , SwitchProps
  , SwitchInnerLabels
  , SwitchLabelPosition(..)

  , switchGroup
  , switchGroup_
  , SwitchGroupProps
  ) where

import Mantine.Core.Prelude

switch :: (SwitchProps -> SwitchProps) -> JSX
switch = mkComponent switchComponent switchToImpl defaultThemingProps_

foreign import switchComponent :: ReactComponent SwitchPropsImpl

type SwitchProps =
  ThemingProps
    ( checked       :: Maybe Boolean
    , color         :: Maybe MantineColor
    , description   :: Maybe JSX
    , error         :: Maybe JSX
    , id            :: Maybe String
    , innerLabels   :: Maybe SwitchInnerLabels
    , label         :: Maybe JSX
    , labelPosition :: Maybe SwitchLabelPosition
    , onChange      :: CheckerHandler
    , radius        :: Maybe MantineNumberSize
    , size          :: Maybe MantineSize
    , thumbIcon     :: Maybe JSX
    , value         :: Maybe String
    )

type SwitchInnerLabels =
  { on  :: JSX
  , off :: JSX
  }

type SwitchPropsImpl =
  ThemingPropsImpl
    ( checked       :: Nullable Boolean
    , color         :: Nullable String
    , description   :: Nullable JSX
    , error         :: Nullable JSX
    , id            :: Nullable String
    , label         :: Nullable JSX
    , labelPosition :: Nullable String
    , offLabel      :: Nullable JSX
    , onChange      :: EffectFn1 SyntheticEvent Unit
    , onLabel       :: Nullable JSX
    , radius        :: Nullable MantineNumberSizeImpl
    , size          :: Nullable String
    , thumbIcon     :: Nullable JSX
    , value         :: Nullable String
    )

data SwitchLabelPosition
  = SwitchLabelPositionLeft
  | SwitchLabelPositionRight

instance ToFFI SwitchLabelPosition String where
  toNative = case _ of
    SwitchLabelPositionLeft  -> "left"
    SwitchLabelPositionRight -> "right"

switchToImpl :: SwitchProps -> SwitchPropsImpl
switchToImpl =
  let flattenLabels props =
        delete (Proxy :: Proxy "innerLabels") props `union`
          { offLabel: _.off <$> props.innerLabels
          , onLabel:  _.on  <$> props.innerLabels
          }
   in toNative <<< flattenLabels

switchGroup :: (SwitchGroupProps -> SwitchGroupProps) -> JSX
switchGroup = mkTrivialComponent switchGroupComponent

switchGroup_ :: Array JSX -> JSX
switchGroup_ children = switchGroup _ { children = children }

foreign import switchGroupComponent :: ReactComponent SwitchGroupPropsImpl

type SwitchGroupProps =
  ThemingProps
    ( children     :: Array JSX
    , defaultValue :: Maybe (Array String)
    , description  :: Maybe JSX
    , error        :: Maybe JSX
    , label        :: Maybe JSX
    , offset       :: Maybe MantineNumberSize
    , onChange     :: ValueHandler (Array String)
    , orientation  :: Maybe Orientation
    , required     :: Maybe Boolean
    , size         :: Maybe MantineSize
    , spacing      :: Maybe MantineNumberSize
    , value        :: Maybe (Array String)
    , withAsterisk :: Maybe Boolean
    )

type SwitchGroupPropsImpl =
  ThemingPropsImpl
    ( children     :: Array JSX
    , defaultValue :: Nullable (Array String)
    , description  :: Nullable JSX
    , error        :: Nullable JSX
    , label        :: Nullable JSX
    , offset       :: Nullable MantineNumberSizeImpl
    , onChange     :: EffectFn1 (Array String) Unit
    , orientation  :: Nullable String
    , required     :: Nullable Boolean
    , size         :: Nullable String
    , spacing      :: Nullable MantineNumberSizeImpl
    , value        :: Nullable (Array String)
    , withAsterisk :: Nullable Boolean
    )
