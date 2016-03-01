{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module Collections where

import           BasicPrelude
import           Control.Lens
import           Data.Aeson.Types
import           Data.Char
import qualified Data.Map.Lazy    as M
import           GHC.Generics     (Generic)

data CollectionItem = CollectionItem
    { _collectionItemId    :: Text
    , _collectionItemName  :: Text
    , _collectionItemUrl   :: Text
    , _collectionItemCover :: Text
    } deriving (Generic)

instance FromJSON CollectionItem where
    parseJSON (Object v) = do
        cId    <- v .: "id"
        cName  <- v .: "name"
        cUrl   <- v .:? "url"   .!= ("/post/" ++ cId ++ "/")
        cCover <- v .:? "cover" .!= ("/images/covers/" ++ cId ++ ".jpg")
        return CollectionItem
            { _collectionItemId    = cId
            , _collectionItemName  = cName
            , _collectionItemUrl   = cUrl
            , _collectionItemCover = cCover
            }

    parseJSON invalid = typeMismatch "CollectionItem" invalid

makeLenses ''CollectionItem

data Collection = Collection
    { _collectionName  :: Text
    , _collectionItems :: [CollectionItem]
    } deriving (Generic)

instance FromJSON Collection where
    -- remove "_collection" from key
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = drop 11 . map toLower
        }

makeLenses ''Collection

type Collections = M.Map Text Collection
