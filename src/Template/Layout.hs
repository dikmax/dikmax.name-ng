{-# LANGUAGE OverloadedStrings #-}

module Template.Layout (layout) where

import           Data.Text.Lazy
import           Lucid

layout :: Html () -> Html ()
layout content = doctypehtml_ $ do
    head_ $ do
        meta_ [charset_ "UTF-8"]
        title_ "Document" -- TODO
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
        link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/styles.css"]
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
