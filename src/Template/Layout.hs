module Template.Layout (defaultLayout, mapLayout, ampLayout) where

import           BasicPrelude
import           Config
import           Control.Lens
import           JsonLD
import           Lucid
import           Lucid.AMP
import           Types

layout :: Html () -> CommonData -> FileMeta -> Html () -> Html ()
layout scripts cd meta content = doctypehtml_ $ do
    head_ $ do
        title_ $ toHtml $ pageTitle meta
        meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
        meta_ [httpEquiv_ "X-UA-Compatible", content_ "IE=edge"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0, ya-title=fade, ya-dock=fade"]

        -- Favicons
        link_ [rel_ "apple-touch-icon", sizes_ "180x180", href_ "/apple-touch-icon.png?v=yyyEB94O8G"]
        link_ [rel_ "icon", type_ "image/png", href_ "/favicon-32x32.png?v=yyyEB94O8G", sizes_ "32x32"]
        link_ [rel_ "icon", type_ "image/png", href_ "/favicon-16x16.png?v=yyyEB94O8G", sizes_ "16x16"]
        link_ [rel_ "manifest", href_ "/manifest.json?v=yyyEB94O8G"]
        link_ [rel_ "mask-icon", href_ "/safari-pinned-tab.svg?v=yyyEB94O8G", term "color" "#474747"]
        link_ [rel_ "shortcut icon", href_ "/favicon.ico?v=yyyEB94O8G"]
        meta_ [name_ "theme-color", content_ "#474747"]

        -- Yandex
        link_ [rel_ "yandex-tableau-widget", href_ "/yandex-widget-manifest.json"]

        -- Resource hints
        link_ [rel_ "preconnect", href_ "https://fonts.googleapis.com/"]
        link_ [rel_ "preconnect", href_ "https://www.gstatic.com/"]
        link_ [rel_ "preconnect", href_ "https://dikmax.disqus.com/"]
        link_ [rel_ "preconnect", href_ "https://ssl.google-analytics.com/"]
        link_ [rel_ "preconnect", href_ "https://a.disquscdn.com/"]

        maybe mempty
            (\_ -> link_ [rel_ "amphtml", href_ $ meta ^. postUrl ++ "amp/"])
            (meta ^? postId)

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

        {-
        toHtmlRaw ("<!--[if lt IE 9]>\
            \<script src=\"/js/html5shiv.js\"></script>\
            \<script src=\"/js/respond.min.js\"></script>\
            \<![endif]-->" :: Text)
        -}

        meta_ [name_ "keywords", content_ keywordsString]
        meta_ [name_ "author", content_ "Maxim Dikun"]
        meta_ [term "property" "author", content_ "1201794820"]
        meta_ [name_ "title", content_ $ pageTitle meta]

        ldMeta meta

        ogMeta meta

        googleAnalytics

    body_ $ do
        content
        footer
        scripts

    where
        keywords :: [Text]
        keywords = meta ^. postTags

        keywordsString :: Text
        keywordsString = intercalate ", " keywords

pageTitle :: FileMeta -> Text
pageTitle meta = case meta ^. postTitle of
    "" -> "[dikmax's blog]"
    a  -> a ++ " :: [dikmax's blog]"


defaultLayout :: CommonData -> FileMeta -> Html () -> Html ()
defaultLayout = layout (script_ [type_ "text/javascript", src_ "/scripts/main.js"] ("" :: Text))


mapLayout :: CommonData -> FileMeta -> Html () -> Html ()
mapLayout = layout (script_ [type_ "text/javascript", src_ "/scripts/map.js"] ("" :: Text))


ampLayout :: CommonData -> FileMeta -> Html () -> Html ()
ampLayout cd meta content = ampDoctypeHtml_ $ do
    head_ $ do
        meta_ [charset_ "utf-8"]
        title_ $ toHtml $ pageTitle meta
        link_ [rel_ "canonical", href_ $ meta ^. postUrl]
        meta_ [name_ "viewport", content_ "width=device-width,minimum-scale=1,initial-scale=1"]
        link_
            [ rel_ "stylesheet"
            , type_ "text/css"
            , href_ "https://fonts.googleapis.com/css?family=Roboto:400,500,500italic&subset=latin,cyrillic"]
        style_ [term "amp-custom" ""] (cd ^. dataCss)
        script_ [ async_ ""
                , term "custom-element" "amp-analytics"
                , src_ "https://cdn.ampproject.org/v0/amp-analytics-0.1.js"] ("" :: Text)
        script_ [ async_ ""
                , term "custom-element" "amp-iframe"
                , src_ "https://cdn.ampproject.org/v0/amp-iframe-0.1.js"] ("" :: Text)
        script_ [ async_ ""
                , term "custom-element" "amp-sidebar"
                , src_ "https://cdn.ampproject.org/v0/amp-sidebar-0.1.js"] ("" :: Text)
        script_ [ async_ ""
                , term "custom-element" "amp-youtube"
                , src_ "https://cdn.ampproject.org/v0/amp-youtube-0.1.js"] ("" :: Text)

        ldMeta meta

        ogMeta meta

        ampBoilerplate_

    body_ $ do
        ampAnalytics_ [type_ "googleanalytics"] $
            script_ [type_ "application/json"]
                ("{\
                   \\"vars\":{\
                     \\"account\":\"" ++ googleAnalyticsUA ++ "\"\
                   \},\
                   \\"triggers\": {\
                     \\"trackPageview\": {\
                       \\"on\":\"visible\",\
                       \\"request\":\"pageview\",\
                       \\"vars\":{\
                           \\"title\":\"" ++ pageTitle meta ++ "\",\
                           \\"ampdocUrl\":\"" ++ (meta ^. postUrl) ++ "\"\
                       \}\
                     \}\
                   \}\
                 \}" :: Text)
        content
        footer

ogMeta :: FileMeta -> Html ()
ogMeta meta = do
    meta_ [term "property" "og:site_name", content_ "[dikmax's blog]"]
    meta_ [term "property" "og:title", content_ $ pageTitle meta]

    meta_ [term "property" "og:url", content_ $ meta ^. postUrl]

    maybe mempty
        (\img -> meta_ [term "property" "og:image", content_ $ domain ++ img]) $
        meta ^. postCover ^. coverImg

    {- TODO
    <meta property="og:description" content="$meta.description$" />
    <meta name="description" content="$meta.description$" />
    <meta itemprop="description" content="$meta.description$" />
    -}
    meta_ [term "property" "og:locale", content_ "ru_BY"]
    meta_ [term "property" "fb:profile_id", content_ "1201794820"]


ldMeta :: FileMeta -> Html ()
ldMeta meta =
    script_ [type_ "application/ld+json"] $ toJsonLD $ meta ^?! postMeta


footer :: Html ()
footer =
    footer_ [class_ "footer"] $
        div_ [class_ "footer__container"] $
            toHtmlRaw ("&copy; Максим Дикун, 2012 &mdash; " ++
                show copyrightYear ++
                "<br/>Любимый корректор: Анастасия Барбосова" :: Text)

googleAnalytics :: Html ()
googleAnalytics =
    toHtmlRaw
        ("<script type=\"text/javascript\">\
             \var _gaq=_gaq||[];\
             \_gaq.push(['_setAccount','" ++ googleAnalyticsUA ++ "']);\
             \_gaq.push(['_setDomainName','dikmax.name']);\
             \_gaq.push(['_trackPageview']);\
             \(function(){\
                 \var ga=document.createElement('script');\
                 \ga.type='text/javascript';\
                 \ga.async=true;\
                 \ga.src=('https:'==document.location.protocol?'https://ssl':'http://www')+'.google-analytics.com/ga.js';\
                 \var s=document.getElementsByTagName('script')[0];\
                 \s.parentNode.insertBefore(ga,s);\
             \})();\
        \</script>" :: Text)
