module Mantine.Core.Buttons.ActionIcon
  ( actionIcon
  , actionIcon_
  , Props_ActionIcon
  , Props_ActionIconImpl
  , ActionIconVariant(..)
  , ActionIconVariantImpl

  , actionIconGroup
  , actionIconGroup_
  , Props_ActionIconGroup
  , Props_ActionIconGroupImpl
  , ActionIconGroupOrientation(..)
  , ActionIconGroupOrientationImpl

  , Props_ActionIconRow
  , Props_ActionIconImplRow

  , Props_ActionIconInner
  , Props_ActionIconInnerImpl
  ) where

import Mantine.Core.Feedback.Loader (Props_LoaderInner, Props_LoaderInnerImpl)
import Mantine.Core.Prelude
import Prim.Row (class Nub)
import React.Icons (icon_)
import React.Icons.Types (ReactIcon)

actionIcon
  :: forall attrs attrs_ attrsImpl attrsImpl_ attrsImpl'
   . Union attrs     attrs_     Props_ActionIcon
  => Union attrsImpl attrsImpl_ Props_ActionIconImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Nub ( children :: Array JSX | attrsImpl) attrsImpl'
  => ReactIcon -> Record attrs -> JSX
actionIcon icon attrs =
  element (unsafeCoerce actionIconComponent) ({ children: [ icon_ icon ] } `merge` toNative attrs)

actionIcon_ :: ReactIcon -> JSX
actionIcon_ icon = actionIcon icon {}

foreign import actionIconComponent :: ReactComponent (Record Props_ActionIconImpl)

type Props_ActionIcon = Props_Common Props_ActionIconRow

type Props_ActionIconRow =
  ( color       :: MantineColor
  , disabled    :: Boolean
  , gradient    :: MantineGradient
  , loaderProps :: Record Props_LoaderInner
  , loading     :: Boolean
  , onClick     :: EventHandler
  , radius      :: MantineNumberSize
  , size        :: MantineNumberSize
  , variant     :: ActionIconVariant
  )

type Props_ActionIconInner =
  ( color       :: Optional MantineColor
  , disabled    :: Optional Boolean
  , gradient    :: Optional MantineGradient
  , loaderProps :: Optional (Record Props_LoaderInner)
  , loading     :: Optional Boolean
  , onClick     :: Optional EventHandler
  , radius      :: Optional MantineNumberSize
  , size        :: Optional MantineNumberSize
  , variant     :: Optional ActionIconVariant
  )

type Props_ActionIconImpl = Props_CommonImpl ( children :: Array JSX | Props_ActionIconImplRow )

type Props_ActionIconImplRow =
  ( color       :: MantineColorImpl
  , disabled    :: Boolean
  , gradient    :: MantineGradientImpl
  , loaderProps :: Record Props_LoaderInnerImpl
  , loading     :: Boolean
  , onClick     :: EventHandler
  , radius      :: MantineNumberSizeImpl
  , size        :: MantineNumberSizeImpl
  , variant     :: ActionIconVariantImpl
  )

type Props_ActionIconInnerImpl =
  ( color       :: OptionalImpl MantineColorImpl
  , disabled    :: OptionalImpl Boolean
  , gradient    :: OptionalImpl MantineGradientImpl
  , loaderProps :: OptionalImpl (Record Props_LoaderInnerImpl)
  , loading     :: OptionalImpl Boolean
  , onClick     :: OptionalImpl EventHandler
  , radius      :: OptionalImpl MantineNumberSizeImpl
  , size        :: OptionalImpl MantineNumberSizeImpl
  , variant     :: OptionalImpl ActionIconVariantImpl
  )

data ActionIconVariant
  = ActionIconOutline
  | ActionIconTransparent
  | ActionIconLight
  | ActionIconDefault
  | ActionIconFilled
  | ActionIconSubtle
  | ActionIconGradient

type ActionIconVariantImpl = OptionalImpl String

instance ToFFI ActionIconVariant ActionIconVariantImpl where
  toNative = toNative <<< Optional <<< case _ of
    ActionIconOutline     -> pure "outline"
    ActionIconTransparent -> pure "transparent"
    ActionIconLight       -> pure "light"
    ActionIconDefault     -> Nothing
    ActionIconFilled      -> pure "filled"
    ActionIconSubtle      -> pure "subtle"
    ActionIconGradient    -> pure "gradient"

actionIconGroup
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_ActionIconGroup
  => Union attrsImpl attrsImpl_ Props_ActionIconGroupImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
actionIconGroup = element (unsafeCoerce actionIconGroupComponent) <<< toNative

actionIconGroup_ :: Array JSX -> JSX
actionIconGroup_ children = actionIconGroup { children }

foreign import actionIconGroupComponent :: ReactComponent (Record Props_ActionIconGroupImpl)

type Props_ActionIconGroup =
  Props_Common
    ( borderWidth :: MantineNumberSize
    , children    :: Array JSX
    , orientation :: ActionIconGroupOrientation
    )

data ActionIconGroupOrientation
  = ActionIconGroupOrientationHorizontal
  | ActionIconGroupOrientationVertical

type ActionIconGroupOrientationImpl = String

instance ToFFI ActionIconGroupOrientation ActionIconGroupOrientationImpl where
  toNative = case _ of
    ActionIconGroupOrientationHorizontal -> "horizontal"
    ActionIconGroupOrientationVertical   -> "vertical"

type Props_ActionIconGroupImpl =
  Props_CommonImpl
    ( borderWidth :: MantineNumberSizeImpl
    , children    :: Array JSX
    , orientation :: ActionIconGroupOrientationImpl
    )
