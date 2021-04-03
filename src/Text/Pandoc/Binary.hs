{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.Pandoc.Binary where

import           Data.Binary (Binary (..))
import           Text.Pandoc

instance Binary Alignment
instance Binary Block
instance Binary Caption
instance Binary Cell
instance Binary Citation
instance Binary CitationMode
instance Binary ColSpan
instance Binary ColWidth
instance Binary Format
instance Binary Inline
instance Binary ListNumberDelim
instance Binary ListNumberStyle
instance Binary MathType
instance Binary Meta
instance Binary MetaValue
instance Binary Pandoc
instance Binary Row
instance Binary RowHeadColumns
instance Binary RowSpan
instance Binary TableBody
instance Binary TableFoot
instance Binary TableHead
instance Binary QuoteType
