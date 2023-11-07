module Mantine.Core.Feedback.Progress
  ( progress
  , ProgressProps

  , progressRoot
  , ProgressRootProps
  , ProgressRootPropsRow

  , progressSection
  , ProgressSectionProps
  , ProgressSectionPropsRow
  ) where

import Mantine.Core.Prelude

progress :: (ProgressProps -> ProgressProps) -> JSX
progress = mkTrivialComponent progressComponent

foreign import progressComponent :: ReactComponent ProgressPropsImpl

progressRoot :: (ProgressRootProps -> ProgressRootProps) -> JSX
progressRoot = mkTrivialComponent progressRootComponent

foreign import progressRootComponent :: ReactComponent ProgressRootPropsImpl

progressSection :: (ProgressSectionProps -> ProgressSectionProps) -> JSX
progressSection = mkTrivialComponent progressSectionComponent

foreign import progressSectionComponent :: ReactComponent ProgressSectionPropsImpl

type ProgressProps =
  ThemingProps (ProgressSectionPropsRow ProgressRootPropsRow)

type ProgressRootPropsRow =
  ( radius :: Maybe MantineNumberSize
  , size   :: Maybe MantineNumberSize
  )

type ProgressRootProps = ThemingProps ProgressRootPropsRow

type ProgressSectionProps =
  ThemingProps
    ( ProgressSectionPropsRow
      ( onMouseEnter :: Maybe EventHandler
      , onMouseLeave :: Maybe EventHandler
      , withAria     :: Boolean
      )
    )

type ProgressSectionPropsRow rest =
  ( animated :: Boolean
  , color    :: Maybe MantineColor
  , striped  :: Boolean
  , value    :: Number
  | rest
  )

type ProgressPropsImpl =
  ThemingPropsImpl (ProgressSectionPropsImplRow ProgressRootPropsImplRow)

type ProgressRootPropsImplRow =
  ( radius :: Nullable MantineNumberSizeImpl
  , size   :: Nullable MantineNumberSizeImpl
  )

type ProgressRootPropsImpl = ThemingPropsImpl ProgressRootPropsImplRow

type ProgressSectionPropsImpl =
  ThemingPropsImpl
    ( ProgressSectionPropsImplRow
      ( onMouseEnter :: Nullable EventHandler
      , onMouseLeave :: Nullable EventHandler
      , withAria     :: Boolean
      )
    )

type ProgressSectionPropsImplRow rest =
  ( animated :: Boolean
  , color    :: Nullable String
  , striped  :: Boolean
  , value    :: Number
  | rest
  )
