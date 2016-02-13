module Text.Pandoc.Lens
    (
    body,
    meta
    ) where

import Control.Lens
import Text.Pandoc.Definition

-- | The body of a pandoc document
body :: Lens' Pandoc [Block]
body = lens (\(Pandoc _ b)->b) (\(Pandoc m _) b->Pandoc m b)

meta :: Lens' Pandoc Meta
meta = lens (\(Pandoc m _)->m) (\(Pandoc _ b) m->Pandoc m b)
