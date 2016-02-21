{-# LANGUAGE OverloadedStrings #-}

module Template.Layout (layout) where

import           Control.Lens
import           Data.Text.Lazy
import           Lucid
import           Types

layout :: CommonData -> Html () -> Html ()
layout cd content = doctypehtml_ $ do
    head_ $ do
        title_ "Document" -- TODO
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

        style_ [type_ "text/css"] (cd ^. dataCss)
        link_
            [ rel_ "stylesheet"
            , type_ "text/css"
            , href_ "https://fonts.googleapis.com/css?family=Roboto:400,500,500italic&subset=latin,cyrillic"]
    body_ $ do
        content

        footer_ [class_ "footer"] $
            div_ [class_ "footer__container"] $
                toHtmlRaw ("&copy; Максим Дикун, 2012 &mdash; 2016<br/>\
                    \Любимый корректор: Анастасия Барбосова" :: Text)
