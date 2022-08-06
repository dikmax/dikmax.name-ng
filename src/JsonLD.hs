{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module JsonLD where

import           BasicPrelude
import           Config
import           Control.Lens
import           Data.Aeson.Types
import           Data.Aeson.Text
import           Data.Binary
import           Data.Char
import qualified Data.Aeson.KeyMap      as KM
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


data ImageObject = ImageObject
    { _imageObjectUrl :: Text
    , _imageObjectHeight :: Int
    , _imageObjectWidth :: Int
    } deriving (Generic, Show)

makeLenses ''ImageObject

instance ToJSON ImageObject where
    toJSON v = case genericToJSON (genericOptions 12) v of
        Object o -> Object $ KM.insert "@type" "ImageObject" o
        _ -> error "Wrong type"

instance Binary ImageObject


newtype Person = Person
    { _personName :: Text
    } deriving (Generic, Show)

makeLenses ''Person

instance ToJSON Person where
    toJSON v = case genericToJSON (genericOptions 7) v of
        Object o -> Object $ KM.insert "@type" "Person" o
        _ -> error "Wrong type"

instance Binary Person

data Organization = Organization
    { _organizationName :: Text
    , _organizationLogo :: ImageObject
    } deriving (Generic, Show)

makeLenses ''Organization

instance ToJSON Organization where
    toJSON v = case genericToJSON (genericOptions 13) v of
        Object o -> Object $ KM.insert "@type" "Organization" o
        _ -> error "Wrong type"

instance Binary Organization


data BlogPosting = BlogPosting
    { _blogPostingHeadline :: Text
    , _blogPostingDatePublished :: UTCTime
    , _blogPostingDateModified :: Maybe UTCTime
    , _blogPostingAuthor :: Person
    , _blogPostingImage :: ImageObject
    , _blogPostingPublisher :: Organization
    , _blogPostingMainEntityOfPage :: Text
    , _blogPostingCopyrightHolder :: Person
    , _blogPostingCopyrightYear :: Int
    , _blogPostingKeywords :: Text
    , _blogPostingEditor :: Person
    } deriving (Generic, Show)

makeLenses ''BlogPosting

instance ToJSON BlogPosting where
    toJSON v = Object $ KM.fromList
        [ ("@type", "BlogPosting")
        , ("headline", String $ v ^. blogPostingHeadline)
        , ("datePublished", formatDate $ v ^. blogPostingDatePublished)
        , ("dateModified", formatDate $ fromMaybe (v ^. blogPostingDatePublished) $
                v ^. blogPostingDateModified)
        , ("author", toJSON $ v ^. blogPostingAuthor)
        , ("image", toJSON $ v ^. blogPostingImage)
        , ("publisher", toJSON $ v ^. blogPostingPublisher)
        , ("mainEntityOfPage", toJSON $ v ^. blogPostingMainEntityOfPage)
        , ("copyrightHolder", toJSON $ v ^. blogPostingCopyrightHolder)
        , ("copyrightYear", toJSON $ v ^. blogPostingCopyrightYear)
        , ("keywords", toJSON $ v ^. blogPostingKeywords)
        , ("editor", toJSON $ v ^. blogPostingEditor)
        ]
        where
            formatDate date =
                String $ T.pack $
                    formatTime timeLocale (iso8601DateFormat (Just "%H:%M:%S%z"))
                    date

instance Binary BlogPosting

data WebPage = WebPage
    { _webPageHeadline :: Text
    , _webPageCopyrightHolder :: Person
    , _webPageCopyrightYear :: Int
    } deriving (Generic, Show)

makeLenses ''WebPage

instance ToJSON WebPage where
    toJSON v = case genericToJSON (genericOptions 8) v of
        Object o -> Object $ KM.insert "@type" "WebPage" o
        _ -> error "Wrong type"

instance Binary WebPage

data AboutPage = AboutPage
    { _aboutPageHeadline :: Text
    , _aboutPageCopyrightHolder :: Person
    , _aboutPageCopyrightYear :: Int
    } deriving (Generic, Show)

makeLenses ''AboutPage

instance ToJSON AboutPage where
    toJSON v = case genericToJSON (genericOptions 10) v of
        Object o -> Object $ KM.insert "@type" "AboutPage" o
        _ -> error "Wrong type"

instance Binary AboutPage

-- Metadata

data Metadata = MAboutPage AboutPage
              | MBlogPosting BlogPosting
              | MImageObject ImageObject
              | MOrganization Organization
              | MPerson Person
              | MWebPage WebPage deriving (Generic, Show)

instance Binary Metadata

class ToMetadata a where
    toMetadata :: a -> Metadata

instance ToMetadata AboutPage where
    toMetadata = MAboutPage

instance ToMetadata BlogPosting where
    toMetadata = MBlogPosting

instance ToMetadata ImageObject where
    toMetadata = MImageObject

instance ToMetadata Person where
    toMetadata = MPerson

instance ToMetadata Organization where
    toMetadata = MOrganization

instance ToMetadata WebPage where
    toMetadata = MWebPage

-- Output

toJsonLD :: Metadata -> Text
toJsonLD (MAboutPage v) = toJsonLD' v
toJsonLD (MBlogPosting v) = toJsonLD' v
toJsonLD (MImageObject v) = toJsonLD' v
toJsonLD (MOrganization v) = toJsonLD' v
toJsonLD (MPerson v) = toJsonLD' v
toJsonLD (MWebPage v) = toJsonLD' v

toJsonLD' :: (ToJSON a) => a -> Text
toJsonLD' v = case toJSON v of
    Object o -> toStrict $ toLazyText $ encodeToTextBuilder $
        Object $ KM.insert "@context" "http://schema.org" o
    _ -> ""

copyrightHolder :: Person
copyrightHolder = Person
    { _personName = "Максим Дикун"
    }

editor :: Person
editor = Person
    { _personName = "Анастасия Барбосова"
    }
