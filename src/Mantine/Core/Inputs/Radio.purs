module Mantine.Core.Inputs.Radio
  ( radio
  , RadioProps
  , RadioLabelPosition(..)

  , radioGroup
  , RadioGroupProps
  ) where

import Mantine.Core.Prelude

radio :: (RadioProps -> RadioProps) -> JSX
radio = mkComponentWithDefault radioComponent defaultRadioProps

foreign import radioComponent :: ReactComponent RadioPropsImpl

type RadioProps =
  ThemingProps
    ( children           :: Array JSX
    , checked            :: Maybe Boolean
    , color              :: Maybe MantineColor
    , defaultChecked     :: Maybe Boolean
    , error              :: Maybe JSX
    , id                 :: Maybe String
    , label              :: Maybe JSX
    , labelPosition      :: Maybe RadioLabelPosition
    , onChange           :: CheckerHandler
    , size               :: MantineSize
    , transitionDuration :: Maybe Number
    , value              :: Maybe String
    )

defaultRadioProps :: RadioProps
defaultRadioProps = defaultThemingProps { size: Small }

type RadioPropsImpl =
  ThemingPropsImpl
    ( children           :: Array JSX
    , checked            :: Nullable Boolean
    , color              :: Nullable String
    , defaultChecked     :: Nullable Boolean
    , error              :: Nullable JSX
    , id                 :: Nullable String
    , label              :: Nullable JSX
    , labelPosition      :: Nullable String
    , onChange           :: EffectFn1 SyntheticEvent Unit
    , size               :: String
    , transitionDuration :: Nullable Number
    , value              :: Nullable String
    )

data RadioLabelPosition
  = RadioLabelPositionLeft
  | RadioLabelPositionRight

instance ToFFI RadioLabelPosition String where
  toNative = case _ of
    RadioLabelPositionLeft  -> "left"
    RadioLabelPositionRight -> "right"

radioGroup :: (RadioGroupProps -> RadioGroupProps) -> JSX
radioGroup = mkComponentWithDefault radioGroupComponent defaultRadioGroupProps

foreign import radioGroupComponent :: ReactComponent RadioGroupPropsImpl

type RadioGroupProps =
  ThemingProps
    ( children     :: Array JSX
    , defaultValue :: Maybe String
    , description  :: Maybe JSX
    , error        :: Maybe JSX
    , label        :: Maybe JSX
    , name         :: Maybe String
    , offset       :: Maybe MantineNumberSize
    , onChange     :: ValueHandler String
    , orientation  :: Orientation
    , required     :: Boolean
    , size         :: MantineSize
    , spacing      :: Maybe MantineNumberSize
    , value        :: Maybe String
    , withAsterisk :: Boolean
    )

defaultRadioGroupProps :: RadioGroupProps
defaultRadioGroupProps =
  defaultThemingProps
    { orientation:  Horizontal
    , size:         Small
    }

type RadioGroupPropsImpl =
  ThemingPropsImpl
    ( children     :: Array JSX
    , defaultValue :: Nullable String
    , description  :: Nullable JSX
    , error        :: Nullable JSX
    , label        :: Nullable JSX
    , name         :: Nullable String
    , offset       :: Nullable MantineNumberSizeImpl
    , onChange     :: EffectFn1 String Unit
    , orientation  :: String
    , required     :: Boolean
    , size         :: String
    , spacing      :: Nullable MantineNumberSizeImpl
    , value        :: Nullable String
    , withAsterisk :: Boolean
    )
