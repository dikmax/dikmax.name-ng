module Template.Layout (defaultLayout, ampLayout) where

import           BasicPrelude
import           Control.Lens
import           Lucid
import           Lucid.AMP
import           Types

defaultLayout :: CommonData -> FileMeta -> Html () -> Html ()
defaultLayout cd meta content = doctypehtml_ $ do
    head_ $ do
        title_ $ toHtml title
        meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
        meta_ [httpEquiv_ "X-UA-Compatible", content_ "IE=edge"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0, ya-title=fade, ya-dock=fade"]

        -- Favicons
        link_ [rel_ "apple-touch-icon", sizes_ "57x57", href_ "/apple-touch-icon-57x57.png?v=NmYO8WoKWA"]
        link_ [rel_ "apple-touch-icon", sizes_ "60x60", href_ "/apple-touch-icon-60x60.png?v=NmYO8WoKWA"]
        link_ [rel_ "apple-touch-icon", sizes_ "72x72", href_ "/apple-touch-icon-72x72.png?v=NmYO8WoKWA"]
        link_ [rel_ "apple-touch-icon", sizes_ "76x76", href_ "/apple-touch-icon-76x76.png?v=NmYO8WoKWA"]
        link_ [rel_ "apple-touch-icon", sizes_ "114x114", href_ "/apple-touch-icon-114x114.png?v=NmYO8WoKWA"]
        link_ [rel_ "apple-touch-icon", sizes_ "120x120", href_ "/apple-touch-icon-120x120.png?v=NmYO8WoKWA"]
        link_ [rel_ "apple-touch-icon", sizes_ "144x144", href_ "/apple-touch-icon-144x144.png?v=NmYO8WoKWA"]
        link_ [rel_ "apple-touch-icon", sizes_ "152x152", href_ "/apple-touch-icon-152x152.png?v=NmYO8WoKWA"]
        link_ [rel_ "apple-touch-icon", sizes_ "180x180", href_ "/apple-touch-icon-180x180.png?v=NmYO8WoKWA"]
        link_ [rel_ "icon", type_ "image/png", href_ "/favicon-32x32.png?v=NmYO8WoKWA", sizes_ "32x32"]
        link_ [rel_ "icon", type_ "image/png", href_ "/android-chrome-192x192.png?v=NmYO8WoKWA", sizes_ "192x192"]
        link_ [rel_ "icon", type_ "image/png", href_ "/favicon-96x96.png?v=NmYO8WoKWA", sizes_ "96x96"]
        link_ [rel_ "icon", type_ "image/png", href_ "/favicon-16x16.png?v=NmYO8WoKWA", sizes_ "16x16"]
        link_ [rel_ "manifest", href_ "/manifest.json?v=NmYO8WoKWA"]
        meta_ [name_ "msapplication-TileColor", content_ "#474747"]
        meta_ [name_ "msapplication-TileImage", content_ "/mstile-144x144.png?v=NmYO8WoKWA"]
        meta_ [name_ "theme-color", content_ "#474747"]

        -- Yandex
        link_ [rel_ "yandex-tableau-widget", href_ "/yandex-widget-manifest.json"]

        -- Resource hints
        link_ [rel_ "preconnect", href_ "https://fonts.googleapis.com/"]
        link_ [rel_ "preconnect", href_ "https://www.gstatic.com/"]
        link_ [rel_ "preconnect", href_ "https://dikmax.disqus.com/"]
        link_ [rel_ "preconnect", href_ "https://ssl.google-analytics.com/"]
        link_ [rel_ "preconnect", href_ "https://a.disquscdn.com/"]
        {-
        link_ [rel_ "dns-prefetch", href_ "//ajaxhttpheaders2.appspot.com/"]
        link_ [rel_ "dns-prefetch", href_ "//translate.google.com/"]
        link_ [rel_ "dns-prefetch", href_ "//translate.googleapis.com/"]
        -}

        link_
            [ rel_ "stylesheet"
            , type_ "text/css"
            , href_ "https://fonts.googleapis.com/css?family=Roboto:400,500,500italic&subset=latin,cyrillic"]
        style_ [type_ "text/css"] (cd ^. dataCss)
        {-
        <link rel="stylesheet" type="text/css" media="print" href="/css/print.css" />
        -}
        link_ [rel_ "alternate", type_ "application/rss+xml", title_ "Лента",
            href_ "/feed.rss"]

        toHtmlRaw ("<!--[if lt IE 9]>\
            \<script src=\"/js/html5shiv.js\"></script>\
            \<script src=\"/js/respond.min.js\"></script>\
            \<![endif]-->" :: Text)

        meta_ [name_ "keywords", content_ keywordsString]
        meta_ [itemprop_ "keywords", content_ keywordsString]
        meta_ [name_ "author", content_ "Maxim Dikun"]
        meta_ [term "property" "author", content_ "1201794820"]
        meta_ [term "property" "og:site_name", content_ "[dikmax's blog]"]
        meta_ [term "property" "og:title", content_ title]
        meta_ [name_ "title", content_ title]
        meta_ [itemprop_ "title", content_ title]

        meta_ [term "property" "og:url", content_ $ meta ^. postUrl]

        {- TODO
        <meta property="og:description" content="$meta.description$" />
        <meta name="description" content="$meta.description$" />
        <meta itemprop="description" content="$meta.description$" />
        -}
        meta_ [term "property" "og:locale", content_ "ru_BY"]
        meta_ [term "property" "fb:profile_id", content_ "1201794820"]

    body_ $ do
        content
        footer
        script_ [type_ "text/javascript", src_ "/scripts/main.js"] ("" :: Text)

    where
        title :: Text
        title = case meta ^. postTitle of
            "" -> "[dikmax's blog]"
            a  -> a ++ " :: [dikmax's blog]"

        keywords :: [Text]
        keywords = meta ^. postTags

        keywordsString :: Text
        keywordsString = intercalate ", " keywords




ampLayout :: CommonData -> FileMeta -> Html () -> Html ()
ampLayout cd meta content = ampDoctypeHtml_ $ do
    head_ $ do
        meta_ [charset_ "utf-8"]
        title_ $ toHtml title
        link_ [rel_ "canonical", href_ $ meta ^. postUrl]
        meta_ [name_ "viewport", content_ "width=device-width,minimum-scale=1,initial-scale=1"]
        script_ [type_ "application/ld+json"] $
            "{\
              \\"@context\": \"http://schema.org\",\
              \\"@type\": \"BlogPosting\",\
              \\"headline\": \"" ++ title ++ "\",\
              \\"datePublished\": \"2015-10-07T12:02:41Z\"\
            \}" -- TODO date
        ampBoilerplate_

        link_
            [ rel_ "stylesheet"
            , type_ "text/css"
            , href_ "https://fonts.googleapis.com/css?family=Roboto:400,500,500italic&subset=latin,cyrillic"]
        style_ [] (cd ^. dataCss)

    body_ $ do
        content
        footer

    where
        title :: Text
        title = case meta ^. postTitle of
            "" -> "[dikmax's blog]"
            a  -> a ++ " :: [dikmax's blog]"


footer :: Html ()
footer =
    footer_ [class_ "footer"] $
        div_ [class_ "footer__container"] $
            toHtmlRaw ("&copy; Максим Дикун, 2012 &mdash; 2016<br/>\
                \Любимый корректор: Анастасия Барбосова" :: Text)
