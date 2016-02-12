{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
module Types where

import           Binary ()
import           Control.Lens
import           Data.Binary
import           Data.Default
import           Data.Hashable
import qualified Data.Map.Lazy              as M
import           GHC.Generics               (Generic)
import           Text.Pandoc

type Posts = M.Map String Pandoc

data PostsCache = PostsCacheById | PostsCacheByDate deriving (Eq)
instance Hashable PostsCache where
    hashWithSalt _ PostsCacheById = 0
    hashWithSalt _ PostsCacheByDate = 1

data PostCoverType = CoverLight | CoverDark deriving (Eq)

data PostCover = PostCover
    { _coverImg     :: Maybe String
    , _coverVCenter :: String
    , _coverHCenter :: String
    , _coverColor   :: Maybe String
    } deriving (Eq)

instance Default PostCover where
    def = PostCover
        { _coverImg     = Nothing
        , _coverVCenter = "center"
        , _coverHCenter = "center"
        , _coverColor   = Nothing
        }

makeLenses ''PostCover

data ImageMeta = ImageMeta
    { imageWidth :: Int
    , imageHeight :: Int
    , imageColor :: String
    , imageThumbnail :: String
    } deriving (Eq, Show, Generic)

instance Binary ImageMeta

instance Default ImageMeta where
    def = ImageMeta
        { imageWidth = 0
        , imageHeight = 0
        , imageColor = ""
        , imageThumbnail = ""
        }
