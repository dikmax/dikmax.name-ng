module Template.Page.Map where

import           BasicPrelude
import           Control.Lens
import qualified Data.Map.Lazy    as M
import           Lucid
import           Map
import           Template.Navigation
import           Template.SvgFlags

mapPage :: (Html () -> Html ()) -> MapCountries -> Html ()
mapPage layout countries = layout $ do
    svgFlagClipPath
    navigation
    div_ [class_ "main main_no-hero"] $
        div_ [class_ "main__centered map__list"] $ do
            forM_ (sortCountries $ M.toList countries) $ \(code, country) -> do
                div_ [class_ "map__subheader", data_ "code" code] $ do
                    maybe noIcon icon $
                        M.lookup code svgFlags
                    div_ [class_ "map__subheader-text"] $
                        toHtml (country ^. countryName)

                forM_ (sortCities $ country ^. countryCities) $ \city -> do
                    div_ [class_ "map__item"] $ do
                        div_ [class_ "map__item-text"] $
                            toHtml (city ^. cityName)

    where
        sortCountries :: [(Text, MapCountry)] -> [(Text, MapCountry)]
        sortCountries =
            sortBy (\a b -> compare (a ^. _2 ^. countryName) (b ^. _2 ^. countryName))

        sortCities :: [MapCity] -> [MapCity]
        sortCities =
            sortBy (\a b -> compare (a ^. cityName) (b ^. cityName))

        noIcon :: Html ()
        noIcon = div_ [class_ "map__subheader-icon map__subheader-icon_none"] mempty

        icon :: Html () -> Html ()
        icon = div_ [class_ $ "map__subheader-icon"]

