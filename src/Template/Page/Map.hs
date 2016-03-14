module Template.Page.Map where

import           BasicPrelude
import           Control.Lens
import qualified Data.Map.Lazy    as M
import           Lucid
import           Map
import           Template.Navigation

mapPage :: (Html () -> Html ()) -> MapCountries -> Html ()
mapPage layout countries = layout $ do
    navigation
    div_ [class_ "main main_no-hero"] $
        div_ [class_ "main__centered archive__list"] $ do
            forM_ (sortCountries $ M.toList countries) $ \(code, country) -> do
                div_ [class_ "archive__subheader", data_ "code" code] $
                    toHtml (country ^. countryName)
                forM_ (sortCities $ country ^. countryCities) $ \city -> do
                    div_ [class_ "archive__item"] $ do
                        div_ [class_ "archive__item-icon archive__item-icon_none"] $
                            mempty
                        div_ [class_ "archive__item-text"] $
                            toHtml (city ^. cityName)

    where
        sortCountries :: [(Text, MapCountry)] -> [(Text, MapCountry)]
        sortCountries =
            sortBy (\a b -> compare (a ^. _2 ^. countryName) (b ^. _2 ^. countryName))

        sortCities :: [MapCity] -> [MapCity]
        sortCities =
            sortBy (\a b -> compare (a ^. cityName) (b ^. cityName))
