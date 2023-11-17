module Mantine.Core.Typography
  ( module Mantine.Core.Typography.Blockquote
  , module Mantine.Core.Typography.Code
  , module Mantine.Core.Typography.Highlight
  , module Mantine.Core.Typography.List
  , module Mantine.Core.Typography.Mark
  , module Mantine.Core.Typography.Table
  , module Mantine.Core.Typography.Text
  , module Mantine.Core.Typography.TypographyStylesProvider
  , module Mantine.Core.Typography.Title
  ) where

import Mantine.Core.Typography.Blockquote (Props_Blockquote, Props_BlockquoteImpl, blockquote, blockquote_)
import Mantine.Core.Typography.Code (Props_Code, Props_CodeImpl, code, code_)
import Mantine.Core.Typography.Highlight (Props_Highlight, Props_HighlightImpl, highlight, highlight_)
import Mantine.Core.Typography.List (ListType(..), ListTypeImpl, Props_List, Props_ListImpl, Props_ListItem, Props_ListItemImpl, list, listItem, listItem_, list_)
import Mantine.Core.Typography.Mark (Props_Mark, Props_MarkImpl, mark)
import Mantine.Core.Typography.Table (Props_Table, Props_TableCaption, Props_TableCaptionImpl, Props_TableImpl, Props_TableScrollContainer, Props_TableScrollContainerImpl, Props_TableTbody, Props_TableTbodyImpl, Props_TableTd, Props_TableTdImpl, Props_TableTfoot, Props_TableTfootImpl, Props_TableTh, Props_TableThImpl, Props_TableThead, Props_TableTheadImpl, Props_TableTr, Props_TableTrImpl, TableCaptionSide(..), TableCaptionSideImpl, WithChildren, WithChildrenImpl, table, tableCaption, tableCaption_, tableScrollContainer, tableScrollContainer_, tableTbody, tableTbody_, tableTd, tableTd_, tableTfoot, tableTfoot_, tableTh, tableTh_, tableThead, tableThead_, tableTr, tableTr_, table_)
import Mantine.Core.Typography.Text (Props_Text, Props_TextBase, Props_TextBaseImpl, Props_TextImpl, Props_TextSpecific, Props_TextSpecificImpl, TextTruncate(..), TextTruncateImpl, text, text_)
import Mantine.Core.Typography.Title (Props_Title, Props_TitleImpl, TitleOrder(..), TitleOrderImpl, title, title1, title2, title3, title4, title5, title6, title_)
import Mantine.Core.Typography.TypographyStylesProvider (typographyStylesProvider_)
