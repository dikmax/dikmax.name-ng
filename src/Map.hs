{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module Map where

import           BasicPrelude
import           Control.Lens
import           Data.Aeson.Types
import           Data.Char
import qualified Data.Map.Lazy    as M
import           GHC.Generics     (Generic)

data Visit = Visit
    { _visitDate :: Text
    , _visitLink :: Maybe Text
    } deriving (Generic)

instance FromJSON Visit where
    -- remove "_visit" from key
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = drop 6 . map toLower
        }

makeLenses ''Visit


data MapCity = MapCity
    { _cityName   :: Text
    , _cityLat    :: Double
    , _cityLon    :: Double
    , _cityVisits :: [Visit]
    } deriving (Generic)

instance FromJSON MapCity where
    parseJSON (Object v) = do
        cName   <- v .:  "name"
        cLat    <- v .:  "lat"
        cLon    <- v .:  "lon"
        cVisits <- v .:? "visits" .!= []
        return MapCity
            { _cityName   = cName
            , _cityLat    = cLat
            , _cityLon    = cLon
            , _cityVisits = cVisits
            }
    parseJSON invalid = typeMismatch "MapCity" invalid

makeLenses ''MapCity


type CountryRegions = M.Map Text Text

data MapCountry = MapCountry
    { _countryName    :: Text
    , _countryColor   :: Text
    , _countryRegions :: Maybe CountryRegions
    , _countryCities  :: [MapCity]
    } deriving (Generic)

instance FromJSON MapCountry where
    -- remove "_country" from key
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = drop 8 . map toLower
        }

makeLenses ''MapCountry


type MapCountries = M.Map Text MapCountry

