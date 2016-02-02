module Binary where

import           Data.Binary (Binary (..))
import           Text.Pandoc

instance Binary Alignment
instance Binary Block
instance Binary Citation
instance Binary CitationMode
instance Binary Format
instance Binary Inline
instance Binary ListNumberDelim
instance Binary ListNumberStyle
instance Binary MathType
instance Binary Meta
instance Binary MetaValue
instance Binary Pandoc
instance Binary QuoteType
