{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types where

import           BasicPrelude
import           Collections
import           Config
import           Control.Lens
import           Data.Binary
import           Data.Default
import qualified Data.Map.Lazy              as M
import           Data.Time
import           GHC.Generics               (Generic)
import           JsonLD
import           Text.Pandoc
import           Text.Pandoc.Binary         ()

-- | Post cover
data PostCover = PostCover
    { _coverImg     :: Maybe Text
    , _coverVCenter :: Text
    , _coverHCenter :: Text
    , _coverSmall   :: Bool
    , _coverColor   :: Maybe Text
    } deriving (Eq, Show, Generic)

instance Default PostCover where
    def = PostCover
        { _coverImg     = Nothing
        , _coverVCenter = "center"
        , _coverHCenter = "center"
        , _coverSmall   = False
        , _coverColor   = Nothing
        }

instance Binary PostCover

makeLenses ''PostCover

-- | Post Meta
data FileMeta =
    PostMeta
    { _postId               :: Text
    , _postMeta             :: Metadata
    , _postTitle            :: Text
    , _postDate             :: Maybe UTCTime
    , _postCover            :: PostCover
    , _postCollections      :: [Text]
    , _postTags             :: [Text]
    , _postUrl              :: Text
    , _postFigureNumbers    :: Bool
    , _postFigureResponsive :: Bool
    } |
    PageMeta
    { _postCover :: PostCover
    , _postMeta  :: Metadata
    , _postTitle :: Text
    , _postTags  :: [Text]
    , _postUrl   :: Text
    } deriving (Show, Generic)

instance Binary FileMeta

instance Default FileMeta where
    def = PageMeta
        { _postCover = def
        , _postMeta  = toMetadata WebPage
            { _webPageHeadline = ""
            , _webPageCopyrightHolder = copyrightHolder
            , _webPageCopyrightYear = copyrightYear
            }
        , _postTitle = ""
        , _postTags  = ["Blog", "блог"]
        , _postUrl   = ""
        }

makeLenses ''FileMeta

-- | Post
data File = File
    { _fileMeta    :: FileMeta
    , _fileContent :: Pandoc
    } deriving (Show, Generic)

instance Binary File

makeLenses ''File

type Posts = M.Map Text File

type PostsTags = M.Map Text Posts

data PostsCache = PostsCacheById | PostsCacheByDate deriving (Eq)
instance Hashable PostsCache where
    hashWithSalt _ PostsCacheById = 0
    hashWithSalt _ PostsCacheByDate = 1

data PostCoverType = CoverLight | CoverDark deriving (Eq)

--
data ImageMeta = ImageMeta
    { _imageWidth     :: Int
    , _imageHeight    :: Int
    , _imageColor     :: Text
    , _imageThumbnail :: Text
    } deriving (Eq, Show, Generic)

instance Binary ImageMeta

makeLenses ''ImageMeta

instance Default ImageMeta where
    def = ImageMeta
        { _imageWidth = 0
        , _imageHeight = 0
        , _imageColor = ""
        , _imageThumbnail = ""
        }

type Images = M.Map Text ImageMeta


-- CommonData
data CommonData = CommonData
    { _dataCss :: Text
    , _imageMeta :: Text -> Maybe ImageMeta
    , _collections :: Collections
    }

instance Default CommonData where
    def = CommonData
        { _dataCss = ""
        , _imageMeta = const Nothing
        , _collections = M.empty
        }

makeLenses ''CommonData

--

data Anything = Anything deriving (Eq)
instance Hashable Anything where
    hashWithSalt _ _ = 0

data SitemapUrl = SitemapUrl
    { _suLoc        :: Text
    , _suChangeFreq :: Text
    , _suPriority   :: Text
    }

instance Default SitemapUrl where
    def = SitemapUrl
        { _suLoc = ""
        , _suChangeFreq = "daily"
        , _suPriority = "1.0"
        }

makeLenses ''SitemapUrl

-- Map related Types

