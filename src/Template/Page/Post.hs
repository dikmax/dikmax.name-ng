module Template.Page.Post (postPage) where

import           BasicPrelude
import           Config
import           Control.Lens
import qualified Data.Text                as T
import           Data.Time
import           Lib
import           Lucid
import           Template.Common
import           Template.Navigation
import           Template.SvgIcons
import           Text.Pandoc
import           Text.Pandoc.LucidWriter
import           Types

postPage :: (Html () -> Html ()) -> CommonData -> File -> Html ()
postPage layout cd post = layout $ do
    header_
        [ class_ "header_for-post"
        , coverToStyle post ] $
        div_ [class_ "header__title-block"] $ do
            div_ [class_ "header__title"] $ toHtml $ post ^. fileMeta ^?! postTitle
            maybe mempty (\time ->
                div_ [data_ "post-date" $ T.pack (formatTime timeLocale (iso8601DateFormat (Just "%H:%M:%S%z")) time)
                    , class_ "header__date"] $ toHtml $ formatTime timeLocale "%A, %-e %B %Y" time) $ post ^. fileMeta ^?! postDate

    navigation

    div_ [class_ "main"] $ do
        div_ [class_ "post"] $ writeLucid ((def :: LucidWriterOptions) & commonData .~ cd) $ post ^. fileContent
        unless (null (post ^. fileMeta ^. postTags)) $ div_ [class_ "main__centered post__meta"] $
            p_ [class_ "post__meta-tags"] $
                mapM_ (\tag -> do
                        a_ [class_ "post__meta-tag", href_ $ tagToUrl tag] $ toHtml tag
                        " ") $
                    post ^. fileMeta ^. postTags


        -- TODO
        div_ [class_ "main__centered share-buttons"] $ do
            a_ [href_ "#", class_ "share-buttons__button share-buttons__button_facebook"] $ do
                iconFacebook
                " Поделиться"
            a_ [href_ "#", class_ "share-buttons__button share-buttons__button_vk"] $ do
                iconVk
                " Расшарить"
            a_ [href_ "#", class_ "share-buttons__button share-buttons__button_google-plus"] $ do
                iconGooglePlus
                " Рассказать"
            a_ [href_ "#", class_ "share-buttons__button share-buttons__button_twitter"] $ do
                iconTwitter
                " Твитнуть"
            a_ [href_ "#", class_ "share-buttons__button share-buttons__button_pinterest"] $ do
                iconPinterest
                " Запинить"
            a_ [href_ "mailto:?subject=", class_ "share-buttons__button share-buttons__button_email"] $ do
                iconEmail
                " Отправить другу"

        div_ [class_ "main__centered related-posts"] $ do
            div_ [class_ "related-posts__collection-name"] $ do
                p_ "Читать ещё"
                p_ [class_ "related-posts__arrow-down"] $ iconArrowDown
            div_ [class_ "related-posts__collection"] mempty -- TODO


        -- TODO
        div_ [id_ "disqus_thread", class_ "main__centered"] mempty
        div_ [class_ "main__centered disqus-post"] $ do
            script_ "var disqus_config = function () {\
                \    this.page.url = 'http://dikmax.name/post/satrip-2015-results/';\
                \    this.page.identifier = 'satrip-2015-results';\
                \    this.page.title = 'SATrip 2015: Итоги';\
                \};\
                \(function() {\
                \    var d = document, s = d.createElement('script');\
                \\
                \    s.src = '//dikmax.disqus.com/embed.js';\
                \\
                \    s.setAttribute('data-timestamp', +new Date());\
                \    (d.head || d.body).appendChild(s);\
                \})();"

            noscript_ $ do
                "Please enable JavaScript to view the "
                a_ [href_ "https://disqus.com/?ref_noscript", rel_ "nofollow"] "comments powered by Disqus."
