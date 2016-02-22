{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types where

import           Control.Lens
import           Data.Binary
import           Data.Default
import           Data.Hashable
import qualified Data.Map.Lazy              as M
import           Data.Time
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
    , _postDate  :: Maybe UTCTime
    , _postCover :: PostCover
    , _postTags  :: [String]
    } |
    PageMeta
    { _postCover :: PostCover
    , _postTitle :: String
    , _postTags  :: [String]
    } deriving (Show, Generic)

instance Binary FileMeta

instance Default FileMeta where
    def = PageMeta
        { _postCover = def
        , _postTitle = ""
        , _postTags = ["Blog", "блог"]
        }

makeLenses ''FileMeta

-- | Post
data File = File
    { _fileMeta    :: FileMeta
    , _fileContent :: Pandoc
    } deriving (Show, Generic)

instance Binary File

makeLenses ''File

type Posts = M.Map String File

type PostsTags = M.Map String (M.Map String File)

data PostsCache = PostsCacheById | PostsCacheByDate deriving (Eq)
instance Hashable PostsCache where
    hashWithSalt _ PostsCacheById = 0
    hashWithSalt _ PostsCacheByDate = 1

data PostCoverType = CoverLight | CoverDark deriving (Eq)

--
data ImageMeta = ImageMeta
    { _imageWidth     :: Int
    , _imageHeight    :: Int
    , _imageColor     :: String
    } deriving (Eq, Show, Generic)

instance Binary ImageMeta

makeLenses ''ImageMeta

instance Default ImageMeta where
    def = ImageMeta
        { _imageWidth = 0
        , _imageHeight = 0
        , _imageColor = ""
        }

-- CommonData
data CommonData = CommonData
    { _dataCss :: String
    , _imageMeta :: String -> Maybe ImageMeta
    }

instance Default CommonData where
    def = CommonData
        { _dataCss = ""
        , _imageMeta = const Nothing
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



-- TODO own metadata format and lenses for it
