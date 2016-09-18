module Template.Page.Map where

import           BasicPrelude
import           Control.Lens
import qualified Data.Map.Lazy    as M
import           Lucid
import           Map
import           Template.Navigation
import           Template.SvgFlags

mapPage :: (Html () -> Html ()) -> Html ()
mapPage layout = layout $ do
    navigation False
    div_ [class_ "main main_no-hero"] $
        div_ [class_ "main__full-width map__view"] mempty

mapListPage :: (Html () -> Html ()) -> MapCountries -> Html ()
mapListPage layout countries = layout $ do
    navigation False
    div_ [class_ "main main_no-hero"] $
        div_ [class_ "main__centered map__list"] $ do
            p_ [] $
                a_ [href_ "/map/"] "Показать карту"
            forM_ (sortCountries $ M.toList countries) $ \(code, country) -> do
                div_ [class_ "map__subheader", data_ "code" code] $ do
                    maybe noIcon icon $
                        M.lookup code svgFlags
                    div_ [id_ $ "country-" ++ code, class_ "map__subheader-text"] $
                        toHtml (country ^. countryName)

                forM_ (sortCities $ country ^. countryCities) $ \city -> do
                    let links = visitLinks (city ^. cityVisits)
                    if null links
                    then
                        span_ [class_ "map__item map__item_empty"] $
                            span_ [class_ "map__item-text"] $
                                toHtml (city ^. cityName)
                    else
                        if length links == 1
                        then
                            a_ [class_ "map__item", href_ $ head links] $
                                span_ [class_ "map__item-text"] $
                                    toHtml (city ^. cityName)
                        else
                            forM_ (withIndexes links) $ \(i, link) ->
                                a_ [class_ "map__item", href_ link] $
                                    span_ [class_ "map__item-text"] $
                                        toHtml (city ^. cityName ++ " (" ++
                                            tshow (i + 1) ++ ")")


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
        icon = div_ [class_ "map__subheader-icon"]

        visitLinks :: [Visit] -> [Text]
        visitLinks = mapMaybe (^. visitLink)

        withIndexes :: [a] -> [(Int, a)]
        withIndexes = withIndexes' 0

        withIndexes' :: Int -> [a] -> [(Int, a)]
        withIndexes' _ [] = []
        withIndexes' i (x : xs) = (i, x) : withIndexes' (i + 1) xs
