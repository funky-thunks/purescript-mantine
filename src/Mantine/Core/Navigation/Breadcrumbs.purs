module Mantine.Core.Navigation.Breadcrumbs
  ( breadcrumbs
  , Props_Breadcrumbs
  , Props_BreadcrumbsImpl
  ) where

import Mantine.Core.Prelude

breadcrumbs
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Breadcrumbs
  => Union attrsImpl attrsImpl_ Props_BreadcrumbsImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
breadcrumbs = element (unsafeCoerce breadcrumbsComponent) <<< toNative

foreign import breadcrumbsComponent :: ReactComponent (Record Props_BreadcrumbsImpl)

type Props_Breadcrumbs =
  Props_Common
    ( children        :: Array JSX
    , separator       :: JSX
    , separatorMargin :: MantineSpacing
    )

type Props_BreadcrumbsImpl =
  Props_CommonImpl
    ( children        :: Array JSX
    , separator       :: JSX
    , separatorMargin :: MantineSpacingImpl
    )
