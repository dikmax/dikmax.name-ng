{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
module Types where

import           Control.Lens
import           Data.Binary
import           Data.Default
import           Data.Hashable
import qualified Data.Map.Lazy              as M
import           GHC.Generics               (Generic)
import           Text.Pandoc
import           Text.Pandoc.Binary ()

-- | Post cover
data PostCover = PostCover
    { _coverImg     :: Maybe String
    , _coverVCenter :: String
    , _coverHCenter :: String
    , _coverColor   :: Maybe String
    } deriving (Eq, Show, Generic)

instance Default PostCover where
    def = PostCover
        { _coverImg     = Nothing
        , _coverVCenter = "center"
        , _coverHCenter = "center"
        , _coverColor   = Nothing
        }

instance Binary PostCover

makeLenses ''PostCover

-- | Post Meta
data FileMeta =
    PostMeta
    { _postId    :: String
    , _postTitle :: String
    , _postDate  :: String -- TODO DateTime
    , _postCover :: PostCover
    , _postTags  :: [String]
    } |
    PageMeta
    { _postCover :: PostCover
    } deriving (Show, Generic)

instance Binary FileMeta

makeLenses ''FileMeta

-- | Post
data File = File
    { _fileMeta :: FileMeta
    , _fileContent :: Pandoc
    } deriving (Show, Generic)

instance Binary File

makeLenses ''File

type Posts = M.Map String File

data PostsCache = PostsCacheById | PostsCacheByDate deriving (Eq)
instance Hashable PostsCache where
    hashWithSalt _ PostsCacheById = 0
    hashWithSalt _ PostsCacheByDate = 1

data PostCoverType = CoverLight | CoverDark deriving (Eq)

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

-- TODO own metadata format and lenses for it
