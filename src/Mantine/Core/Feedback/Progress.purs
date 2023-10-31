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
  MantineComponent (ProgressSectionPropsRow ProgressRootPropsRow)

type ProgressRootPropsRow =
  ( radius :: Maybe MantineNumberSize
  , size   :: Maybe MantineNumberSize
  )

type ProgressRootProps = MantineComponent ProgressRootPropsRow

type ProgressSectionProps =
  MantineComponent
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
  MantineComponentImpl (ProgressSectionPropsImplRow ProgressRootPropsImplRow)

type ProgressRootPropsImplRow =
  ( radius :: Nullable MantineNumberSizeImpl
  , size   :: Nullable MantineNumberSizeImpl
  )

type ProgressRootPropsImpl = MantineComponentImpl ProgressRootPropsImplRow

type ProgressSectionPropsImpl =
  MantineComponentImpl
    ( ProgressSectionPropsImplRow
      ( onMouseEnter :: Nullable EventHandler
      , onMouseLeave :: Nullable EventHandler
      , withAria     :: Boolean
      )
    )

type ProgressSectionPropsImplRow rest =
  ( animated :: Boolean
  , color    :: Nullable MantineColorImpl
  , striped  :: Boolean
  , value    :: Number
  | rest
  )
