{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types where

import           BasicPrelude
import           Collections
import           Control.Lens
import           Data.Binary
import           Data.Default
import qualified Data.Map.Lazy              as M
import           Data.Time
import           GHC.Generics               (Generic)
import           Text.Pandoc
import           Text.Pandoc.Binary         ()

-- | Post cover
data PostCover = PostCover
    { _coverImg     :: Maybe Text
    , _coverVCenter :: Text
    , _coverHCenter :: Text
    , _coverColor   :: Maybe Text
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
    { _postId          :: Text
    , _postTitle       :: Text
    , _postDate        :: Maybe UTCTime
    , _postCover       :: PostCover
    , _postCollections :: [Text]
    , _postTags        :: [Text]
    , _postUrl         :: Text
    } |
    PageMeta
    { _postCover :: PostCover
    , _postTitle :: Text
    , _postTags  :: [Text]
    , _postUrl   :: Text
    } deriving (Show, Generic)

instance Binary FileMeta

instance Default FileMeta where
    def = PageMeta
        { _postCover = def
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
    } deriving (Eq, Show, Generic)

instance Binary ImageMeta

makeLenses ''ImageMeta

instance Default ImageMeta where
    def = ImageMeta
        { _imageWidth = 0
        , _imageHeight = 0
        , _imageColor = ""
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
instance Binary UTCTime where
    put (UTCTime a b) = put a >> put b
    get = UTCTime <$> get <*> get

instance Binary Day where
    put (ModifiedJulianDay d) = put d
    get = ModifiedJulianDay <$> get

instance Binary DiffTime where
    put = put . fromEnum
    get = toEnum <$> get

data Anything = Anything deriving (Eq)
instance Hashable Anything where
    hashWithSalt _ _ = 0
