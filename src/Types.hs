module Types where

import           Data.Hashable
import qualified Data.Map.Lazy              as M
import           Text.Pandoc


type Posts = M.Map String Pandoc

data PostsCache = PostsCacheById | PostsCacheByDate deriving (Eq)
instance Hashable PostsCache where
    hashWithSalt _ PostsCacheById = 0
    hashWithSalt _ PostsCacheByDate = 1
