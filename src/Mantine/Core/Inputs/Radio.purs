module Mantine.Core.Inputs.Radio
  ( radio
  , RadioProps
  , RadioLabelPosition(..)

  , radioGroup
  , RadioGroupProps

  , module Mantine.Core.Inputs.Input
  ) where

import Mantine.Core.Inputs.Input (InputWrapperOrder(..))
import Mantine.Core.Prelude

radio :: (RadioProps -> RadioProps) -> JSX
radio = mkComponentWithDefault radioComponent defaultRadioProps

foreign import radioComponent :: ReactComponent RadioPropsImpl

type RadioProps =
  ThemingProps
    ( checked            :: Maybe Boolean
    , color              :: Maybe MantineColor
    , defaultChecked     :: Maybe Boolean
    , description        :: Maybe JSX
    , error              :: Maybe JSX
    , icon               :: Maybe JSX
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
    ( checked            :: Nullable Boolean
    , color              :: Nullable String
    , defaultChecked     :: Nullable Boolean
    , description        :: Nullable JSX
    , error              :: Nullable JSX
    , icon               :: Nullable JSX
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
radioGroup = mkComponent radioGroupComponent radioGroupToImpl defaultRadioGroupProps

foreign import radioGroupComponent :: ReactComponent RadioGroupPropsImpl

type RadioGroupProps =
  ThemingProps
    ( children          :: Array JSX
    , defaultValue      :: Maybe String
    , description       :: Maybe JSX
    , error             :: Maybe JSX
    , inputContainer    :: Maybe (JSX -> JSX)
    , inputWrapperOrder :: Maybe (Array InputWrapperOrder)
    , label             :: Maybe JSX
    , name              :: Maybe String
    , onChange          :: ValueHandler String
    , required          :: Boolean
    , size              :: MantineSize
    , value             :: Maybe String
    , withAsterisk      :: Boolean
    )

defaultRadioGroupProps :: RadioGroupProps
defaultRadioGroupProps = defaultThemingProps { size: Small }

type RadioGroupPropsImpl =
  ThemingPropsImpl
    ( children          :: Array JSX
    , defaultValue      :: Nullable String
    , description       :: Nullable JSX
    , error             :: Nullable JSX
    , inputContainer    :: Nullable (JSX -> JSX)
    , inputWrapperOrder :: Nullable (Array String)
    , label             :: Nullable JSX
    , name              :: Nullable String
    , onChange          :: EffectFn1 String Unit
    , required          :: Boolean
    , size              :: String
    , value             :: Nullable String
    , withAsterisk      :: Boolean
    )

radioGroupToImpl :: (RadioGroupProps -> RadioGroupPropsImpl)
radioGroupToImpl props =
  let rest = toNative
         <<< delete (Proxy :: Proxy "inputContainer")
      inputContainer = toNullable props.inputContainer
   in { inputContainer } `union` rest props
