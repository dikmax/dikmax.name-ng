module Template.Page.Post (postPage) where

import           BasicPrelude
import           Collections
import           Config
import           Control.Lens
import qualified Data.Map.Lazy            as M
import qualified Data.Text                as T
import           Data.Time
import           Lib
import           Lucid
import           Network.URI
import           Template.Navigation
import           Template.SvgIcons
import           Text.Pandoc
import           Text.Pandoc.LucidWriter
import           Types

postPage :: Bool
         -> (Html () -> Html ())
         -> CommonData
         -> File
         -> Maybe File
         -> Maybe File
         -> Html ()
postPage isAmp layout cd post previousPost nextPost = layout $ do
    header_
        [ class_ $ "header_for-post " ++
            (if post ^. fileMeta ^. postCover ^. coverSmall
            then "header_for-post-small" else "header_for-post-large")
        ] $
        div_ [class_ "header__title-block"] $ do
            div_ [class_ "header__title"] $ toHtml title
            maybe mempty (\time ->
                div_ [data_ "post-date" $
                        T.pack (formatTime timeLocale (iso8601DateFormat (Just "%H:%M:%S%z")) time)
                    , class_ "header__date"] $ toHtml $
                        formatTime timeLocale "%A, %-e %B %Y" time) $ post ^. fileMeta ^?! postDate

    navigation

    div_ [class_ "main"] $ do
        div_ [class_ "post"] $ writeLucid opts $ post ^. fileContent
        unless (null (post ^. fileMeta ^. postTags)) $ div_ [class_ "main__centered post__meta"] $
            p_ [class_ "post__meta-tags"] $
                mapM_ (\tag -> do
                        a_ [class_ "post__meta-tag", href_ $ tagToUrl tag] $ toHtml tag
                        " ") $
                    post ^. fileMeta ^. postTags


        div_ [class_ "main__centered share-buttons"] $ do
            a_ [href_ urlFacebook, target_ "blank",
                    class_ "share-buttons__button share-buttons__button_facebook"] $ do
                iconFacebook
                " Поделиться"
            a_ [href_ urlVk, target_ "blank",
                    class_ "share-buttons__button share-buttons__button_vk"] $ do
                iconVk
                " Расшарить"
            a_ [href_ urlGooglePlus, target_ "blank",
                    class_ "share-buttons__button share-buttons__button_google-plus"] $ do
                iconGooglePlus
                " Рассказать"
            a_ [href_ urlTwitter, target_ "blank",
                    class_ "share-buttons__button share-buttons__button_twitter"] $ do
                iconTwitter
                " Твитнуть"
            a_ [href_ urlPinterest, target_ "blank",
                    class_ "share-buttons__button share-buttons__button_pinterest"] $ do
                iconPinterest
                " Запинить"
            a_ [href_ urlEmail, target_ "blank",
                    class_ "share-buttons__button share-buttons__button_email"] $ do
                iconEmail
                " Отправить другу"

        when (isJust previousPost || isJust nextPost) $
            div_ [class_ "main__centered pager"] $ do
                maybe (span_ [] mempty) (\p ->
                    a_ [ href_ $ postUrlFromId (p ^. fileMeta ^. postId)
                       , class_ "pager__previous"] $
                        toHtml ("← " ++ p ^. fileMeta ^. postTitle)) previousPost
                maybe (span_ [] mempty) (\p ->
                    a_ [ href_ $ postUrlFromId (p ^. fileMeta ^. postId)
                       , class_ "pager__next"] $
                        toHtml (p ^. fileMeta ^. postTitle ++ " →")) nextPost


        unless (null $ post ^. fileMeta ^. postCollections) $
            div_ [class_ "main__full-width__centered related-posts"] $ do
                div_ [class_ "related-posts__collection-name"] $ do
                    p_ "Читать ещё"
                    p_ [class_ "related-posts__arrow-down"] iconArrowDown
                forM_ (post ^. fileMeta ^. postCollections) writeCollection

        unless isAmp disqus
    where
        writeCollection :: Text -> Html ()
        writeCollection collectionId =
            maybe (terror $ "Collection " ++ collectionId ++
                    " not defined for " ++ post ^. fileMeta ^. postId)
                (\c -> do
                    div_ [class_ "related-posts__collection-name"] $
                        p_ $ toHtml $ c ^. collectionName
                    div_ [class_ "related-posts__collection"] $
                        forM_ (take showMaxCount $ c ^. collectionItems)
                            writeCollectionItem
                ) $ M.lookup collectionId (cd ^. collections)

        writeCollectionItem :: CollectionItem -> Html ()
        writeCollectionItem cItem =
            div_ [class_ "related-posts__cover"] $ do
                a_ [ href_ $ cItem ^. collectionItemUrl
                   , class_ "related-posts__cover-image"] $
                   img_ [ src_ $ cItem ^. collectionItemCover
                        , name_ (cItem ^. collectionItemName)]
                a_ [ href_ $ cItem ^. collectionItemUrl
                   , class_ "related-posts__title-background"] $
                   toHtml (cItem ^. collectionItemName)

        showMaxCount :: Int
        showMaxCount
            | length (post ^. fileMeta ^. postCollections) == 1 = 8
            | otherwise = 4


        disqus :: Html ()
        disqus = do
             div_ [id_ "disqus_thread", class_ "main__centered"] mempty
             div_ [class_ "main__centered disqus-post"] $ do
                 script_ $
                     "var disqus_config=function(){\
                         \this.page.url='" ++ escapeJsString url ++ "';\
                         \this.page.identifier='" ++ escapeJsString pId ++ "';\
                         \this.page.title='" ++ escapeJsString title ++ "';\
                     \};\
                     \(function(){\
                         \var d=document,s=d.createElement('script');\
                         \s.src='//dikmax.disqus.com/embed.js';\
                         \s.setAttribute('data-timestamp',+new Date());\
                         \(d.head||d.body).appendChild(s);\
                     \})();"

                 noscript_ $ do
                     "Please enable JavaScript to view the "
                     a_ [ href_ "https://disqus.com/?ref_noscript"
                        , rel_ "nofollow"] "comments powered by Disqus."

        opts :: LucidWriterOptions
        opts = (def :: LucidWriterOptions)
                & commonData        .~ cd
                & renderType        .~ (if isAmp then RenderAMP else RenderNormal)
                & showFigureNumbers .~ (post ^. fileMeta ^?! postFigureNumbers)


        pId :: Text
        pId = post ^. fileMeta ^?! postId

        title :: Text
        title = post ^. fileMeta ^. postTitle

        fullTitle :: Text
        fullTitle = title ++ " :: [dikmax's blog]"

        escapeJsString :: Text -> Text
        escapeJsString = T.replace "'" "\\'"

        escapeURIComponent :: Text -> Text
        escapeURIComponent = T.pack . escapeURIString isUnescapedInURIComponent . T.unpack

        url :: Text
        url = post ^. fileMeta ^. postUrl

        urlEmail :: Text
        urlEmail = "mailto:?subject=" ++ escapeURIComponent fullTitle

        urlFacebook :: Text
        urlFacebook = "https://www.facebook.com/sharer/sharer.php?u="
            ++ escapeURIComponent url

        urlGooglePlus :: Text
        urlGooglePlus = "https://plus.google.com/share?url="
            ++ escapeURIComponent url

        urlPinterest :: Text
        urlPinterest = "https://pinterest.com/pin/create/button/?url=" ++
            escapeURIComponent url ++
            maybe "" (\i -> "&media=" ++ escapeURIComponent i)
                (post ^. fileMeta ^. postCover ^. coverImg) ++
            "&description=" ++ escapeURIComponent fullTitle

        urlTwitter :: Text
        urlTwitter = "https://twitter.com/intent/tweet?url=" ++
            escapeURIComponent url ++
            "&text=" ++ escapeURIComponent fullTitle ++
            "&via=dikmax"

        urlVk :: Text
        urlVk = "https://vk.com/share.php?url=" ++
            escapeURIComponent url ++
            "&title=" ++ escapeURIComponent fullTitle