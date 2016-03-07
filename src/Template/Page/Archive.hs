module Template.Page.Archive where

import           BasicPrelude
import           Config
import           Control.Lens
import qualified Data.Map.Lazy              as M
import qualified Data.Text                  as T
import           Data.Time
import           Lib
import           Lucid
import           Template.Navigation
import           Template.SvgIcons
import           Types

-- TODO SVG "use"

archivePage :: (Html () -> Html ()) -> [File] -> Html ()
archivePage layout files = layout $ do
    navigation
    div_ [class_ "main main_no-hero"] $ do
        div_ [class_ "main__centered archive__list"] $ do
            forM_ sectionsKeys $ \month -> do
                div_ [class_ "archive__subheader"] $
                    toHtml $ monthText $ head (sections M.! month)

                forM_ (sections M.! month) $ \f -> do
                    a_ [class_ "archive__item", href_ $ postIdToUrl $ f ^. fileMeta ^?! postId] $ do
                        findIcon f
                        div_ [class_ "archive__item-text"] $
                            toHtml $ f ^. fileMeta ^. postTitle
    where
        sectionsKeys = sortBy (flip compare) $ M.keys sections
        sections = archiveMonths files

        monthText :: File -> Text
        monthText f = T.pack $ formatTime archiveTimeLocale "%B %Y" $
                fromMaybe (error "") $ f ^. fileMeta ^?! postDate

        noIcon :: Html ()
        noIcon = div_ [class_ "archive__item-icon archive__item-icon_none"] mempty

        icon :: Text -> Html () -> Html ()
        icon cl i = div_ [class_ $ "archive__item-icon archive__item-icon_" ++ cl] i

        findIcon :: File -> Html ()
        findIcon f
            | "путешествие" `elem` (f ^. fileMeta ^?! postTags) =
                icon "map" iconMap
            | "программирование" `elem` (f ^. fileMeta ^?! postTags) =
                icon "code" iconCode
            | "блог" `elem` (f ^. fileMeta ^?! postTags) =
                icon "pages" iconPages
            | otherwise = noIcon
