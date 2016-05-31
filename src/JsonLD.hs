{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module JsonLD where

import           BasicPrelude
import           Config
import           Control.Lens
import           Data.Aeson.Types
import           Data.Aeson.Encode
import           Data.Binary
import           Data.Char
import qualified Data.HashMap.Strict    as HM
import qualified Data.Text              as T
import           Data.Text.Lazy             (toStrict)
import           Data.Text.Lazy.Builder     (toLazyText)
import           Data.Time
import           GHC.Generics               (Generic)

instance Binary UTCTime where
    put (UTCTime a b) = put a >> put b
    get = UTCTime <$> get <*> get

instance Binary Day where
    put (ModifiedJulianDay d) = put d
    get = ModifiedJulianDay <$> get

instance Binary DiffTime where
    put = put . fromEnum
    get = toEnum <$> get

genericFieldModifier :: Int -> String -> String
genericFieldModifier int s = toLower (s !! int) : drop (int + 1) s

genericOptions :: Int -> Options
genericOptions i = defaultOptions
    { fieldLabelModifier = genericFieldModifier i
    }




data Person = Person
    { _personName :: Text
    } deriving (Generic, Show)

makeLenses ''Person

instance ToJSON Person where
    toJSON v = case genericToJSON (genericOptions 7) v of
        Object o -> Object $ HM.insert "@type" "Person" o
        _ -> error "Wrong type"

instance Binary Person

data ImageObject = ImageObject
    { _imageObjectUrl :: Text
    , _imageObjectHeight :: Int
    , _imageObjectWidth :: Int
    } deriving (Generic, Show)

makeLenses ''ImageObject

instance ToJSON ImageObject where
    toJSON v = case genericToJSON (genericOptions 12) v of
        Object o -> Object $ HM.insert "@type" "ImageObject" o
        _ -> error "Wrong type"

instance Binary ImageObject


data BlogPosting = BlogPosting
    { _blogPostingHeadline :: Text
    , _blogPostingDatePublished :: UTCTime
    , _blogPostingDateModified :: Maybe UTCTime
    , _blogPostingAuthor :: Person
    , _blogPostingImage :: ImageObject
    } deriving (Generic, Show)

makeLenses ''BlogPosting

instance ToJSON BlogPosting where
    toJSON v = Object $ HM.fromList
        [ ("@type", "BlogPosting")
        , ("headline", String $ v ^. blogPostingHeadline)
        , ("datePublished", formatDate $ v ^. blogPostingDatePublished)
        , ("dateModified", formatDate $ fromMaybe (v ^. blogPostingDatePublished) $
                v ^. blogPostingDateModified)
        , ("author", toJSON $ v ^. blogPostingAuthor)
        , ("image", toJSON $ v ^. blogPostingImage)
        ]
        where
            formatDate date =
                String $ T.pack $
                    formatTime timeLocale (iso8601DateFormat (Just "%H:%M:%S%z")) $
                    date

    {-case genericToJSON (genericOptions 12) v of
        Object o -> Object $ HM.insert "@type" "BlogPosting" o
        _ -> error "Wrong type"-}

instance Binary BlogPosting

data Metadata = MPerson Person
              | MImageObject ImageObject
              | MBlogPosting BlogPosting deriving (Generic, Show)

instance Binary Metadata

class ToMetadata a where
    toMetadata :: a -> Metadata

instance ToMetadata Person where
    toMetadata = MPerson

instance ToMetadata ImageObject where
    toMetadata = MImageObject

instance ToMetadata BlogPosting where
    toMetadata = MBlogPosting

toJsonLD :: Metadata -> Text
toJsonLD (MPerson v) = toJsonLD' v
toJsonLD (MImageObject v) = toJsonLD' v
toJsonLD (MBlogPosting v) = toJsonLD' v

toJsonLD' :: (ToJSON a) => a -> Text
toJsonLD' v = case toJSON v of
    Object o -> toStrict $ toLazyText $ encodeToTextBuilder $
        Object $ HM.insert "@context" "http://schema.org" o
    _ -> ""
