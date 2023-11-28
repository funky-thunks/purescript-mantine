module Mantine.Core.Feedback.Progress
  ( progress
  , Props_Progress
  , Props_ProgressImpl

  , progressRoot
  , Props_ProgressRoot
  , Props_ProgressRootImpl
  , Props_ProgressRootRow
  , Props_ProgressRootImplRow

  , progressSection
  , Props_ProgressSection
  , Props_ProgressSectionImpl
  , Props_ProgressSectionRow
  , Props_ProgressSectionImplRow

  , progressLabel_
  ) where

import Mantine.Core.Prelude

progress
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Progress
  => Union attrsImpl attrsImpl_ Props_ProgressImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
progress = element (unsafeCoerce progressComponent) <<< toNative

foreign import progressComponent :: ReactComponent (Record Props_ProgressImpl)

progressRoot
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_ProgressRoot
  => Union attrsImpl attrsImpl_ Props_ProgressRootImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
progressRoot = element (unsafeCoerce progressRootComponent) <<< toNative

foreign import progressRootComponent :: ReactComponent (Record Props_ProgressRootImpl)

progressSection
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_ProgressSection
  => Union attrsImpl attrsImpl_ Props_ProgressSectionImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
progressSection = element (unsafeCoerce progressSectionComponent) <<< toNative

foreign import progressSectionComponent :: ReactComponent (Record Props_ProgressSectionImpl)

type Props_Progress =
  Props_Common (Props_ProgressSectionRow Props_ProgressRootRow)

type Props_ProgressRootRow =
  ( children :: Array JSX
  , radius   :: MantineNumberSize
  , size     :: MantineNumberSize
  )

type Props_ProgressRoot = Props_Common Props_ProgressRootRow

type Props_ProgressSection =
  Props_Common
    ( Props_ProgressSectionRow
      ( children     :: Array JSX
      , onMouseEnter :: EventHandler
      , onMouseLeave :: EventHandler
      , withAria     :: Boolean
      )
    )

type Props_ProgressSectionRow rest =
  ( animated :: Boolean
  , color    :: MantineColor
  , striped  :: Boolean
  , value    :: Number
  | rest
  )

type Props_ProgressImpl =
  Props_CommonImpl (Props_ProgressSectionImplRow Props_ProgressRootImplRow)

type Props_ProgressRootImplRow =
  ( children :: Array JSX
  , radius   :: MantineNumberSizeImpl
  , size     :: MantineNumberSizeImpl
  )

type Props_ProgressRootImpl = Props_CommonImpl Props_ProgressRootImplRow

type Props_ProgressSectionImpl =
  Props_CommonImpl
    ( Props_ProgressSectionImplRow
      ( children     :: Array JSX
      , onMouseEnter :: EventHandler
      , onMouseLeave :: EventHandler
      , withAria     :: Boolean
      )
    )

type Props_ProgressSectionImplRow rest =
  ( animated :: Boolean
  , color    :: MantineColorImpl
  , striped  :: Boolean
  , value    :: Number
  | rest
  )

progressLabel :: Props_ProgressLabel -> JSX
progressLabel = element progressLabelComponent

progressLabel_ :: Array JSX -> JSX
progressLabel_ children = progressLabel { children }

foreign import progressLabelComponent :: ReactComponent Props_ProgressLabel

type Props_ProgressLabel = { children :: Array JSX }
