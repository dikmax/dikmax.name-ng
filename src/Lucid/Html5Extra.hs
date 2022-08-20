module Lucid.Html5Extra where

import           BasicPrelude
import           Lucid.Base

-- | @picture@ element
picture_ :: Term arg result => arg -> result
picture_ = term "picture"
