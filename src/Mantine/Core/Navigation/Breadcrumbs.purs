module Mantine.Core.Navigation.Breadcrumbs
  ( breadcrumbs
  , BreadcrumbsProps
  ) where

import Mantine.Core.Prelude

breadcrumbs :: (BreadcrumbsProps -> BreadcrumbsProps) -> JSX
breadcrumbs = mkTrivialComponent breadcrumbsComponent

foreign import breadcrumbsComponent :: ReactComponent BreadcrumbsPropsImpl

type BreadcrumbsProps =
  ThemingProps
    ( children        :: Array JSX
    , separator       :: Maybe JSX
    , separatorMargin :: Maybe MantineSpacing
    )

type BreadcrumbsPropsImpl =
  ThemingPropsImpl
    ( children        :: Array JSX
    , separator       :: Nullable JSX
    , separatorMargin :: Nullable MantineSpacingImpl
    )
