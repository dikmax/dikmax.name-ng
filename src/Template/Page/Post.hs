{-# LANGUAGE OverloadedStrings #-}

module Template.Page.Post (postPage) where

import           Data.Text.Lazy
import           Lib
import           Lucid
import           Template.Common
import           Template.Layout
import           Template.Navigation
import           Text.Pandoc

postPage :: Meta -> Html () -> Html ()
postPage meta content = layout $ do
    let cover = getPostCover meta
    header_
        [ class_ "header"
        , style_ $ toStrict $
            maybe "" (\i -> "background-image:url(" `append` pack i `append` ");") (coverImg cover)
            `append` maybe "" (\i -> "background-color:" `append` pack i `append` ";") (coverColor cover)
            `append` "background-position-x:" `append` pack (coverHCenter cover) `append` ";"
            `append` "background-position-y:" `append` pack (coverVCenter cover)
        ] $
        div_ [class_ "title-block"] $ do
            div_ [class_ "title"] $ toHtml $ pack $ getPostTitle meta
            div_ [data_ "post-date" "2015-10-02T08:25:00+0000", class_ "date"] "Пятница, 2 октября 2015" -- TODO

    navigation

    div_ [class_ "main-container"] $ do
        div_ [class_ "post-body"] content
        div_ [class_ "post-meta"] $ do
            p_ [class_ "tags"] $ do
                a_ [href_ "/tag/satrip/"] "satrip"
                " "
                a_ [href_ "/tag/satrip-2015/"] "satrip-2015"
                " "
                a_ [href_ "/tag/бразилия/"] "бразилия"
                " "
                a_ [href_ "/tag/отпуск/"] "отпуск"
                " "
                a_ [href_ "/tag/путешествие/"] "путешествие"
                " "
                a_ [href_ "/tag/фотки/"] "фотки"

            div_ [class_ "share-buttons"] $ do
                a_ [href_ "#", class_ "facebook"] $ do {i_ [class_ "fa fa-facebook"] mempty; " Поделиться"}
                a_ [href_ "#", class_ "vk"] $ do {i_ [class_ "fa fa-vk"] mempty; " Расшарить"}
                a_ [href_ "#", class_ "google-plus"] $ do {i_ [class_ "fa fa-google-plus"] mempty; " Рассказать"}
                a_ [href_ "#", class_ "twitter"] $ do {i_ [class_ "fa fa-twitter"] mempty; " Твитнуть"}
                a_ [href_ "#", class_ "pinterest"] $ do {i_ [class_ "fa fa-pinterest"] mempty; " Запинить"}
                a_ [href_ "mailto:?subject=", class_ "email"] $ do {i_ [class_ "fa fa-envelope"] mempty; " Отправить другу"}

        div_ [class_ "related-posts"] $ do
            div_ [class_ "collection-name"] $ do
                p_ "Читать ещё"
                p_ $ i_ [class_ "fa fa-arrow-down fa-2x"] mempty
            div_ [class_ "collection"] mempty -- TODO


        div_ [id_ "disqus_thread", class_ "container"] mempty
        div_ [class_ "container disqus-post"] $ do
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
