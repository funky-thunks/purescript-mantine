module Mantine.Core.Typography
  ( module Mantine.Core.Typography.Blockquote
  , module Mantine.Core.Typography.Code
  , module Mantine.Core.Typography.Highlight
  , module Mantine.Core.Typography.List
  , module Mantine.Core.Typography.Mark
  , module Mantine.Core.Typography.Table
  , module Mantine.Core.Typography.Text
  , module Mantine.Core.Typography.Title
  , module Mantine.Core.Typography.TypographyStylesProvider
  ) where

import Mantine.Core.Typography.Blockquote (BlockquoteProps, blockquote, blockquote_)
import Mantine.Core.Typography.Code (CodeProps, code, code_)
import Mantine.Core.Typography.Highlight (HighlightProps, highlight)
import Mantine.Core.Typography.List (ListItemProps, ListProps, ListType(..), list, listItem, listItem_, list_)
import Mantine.Core.Typography.Mark (MarkProps, mark)
import Mantine.Core.Typography.Table (TableCaptionProps, TableCaptionSide(..), TableProps, TableScrollContainerProps, TableTbodyProps, TableTdProps, TableTfootProps, TableThProps, TableTheadProps, TableTrProps, WithChildren, table, tableCaption, tableCaption_, tableScrollContainer, tableScrollContainer_, tableTbody, tableTbody_, tableTd, tableTd_, tableTfoot, tableTfoot_, tableTh, tableTh_, tableThead, tableThead_, tableTr, tableTr_, table_)
import Mantine.Core.Typography.Text (TextProps, TextPropsImplRow, TextPropsRow, TextSpecificPropsRow, TextTruncate(..), TextTruncateImpl, text, text_)
import Mantine.Core.Typography.Title (TitleOrder(..), TitleProps, title, title_, title1, title2, title3, title4, title5, title6)
import Mantine.Core.Typography.TypographyStylesProvider (typographyStylesProvider_)
