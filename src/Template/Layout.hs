{-# LANGUAGE OverloadedStrings #-}

module Template.Layout (layout) where

import           Data.Text.Lazy
import           Lucid

layout :: Html () -> Html ()
layout content = doctypehtml_ $ do
    head_ $ do
        meta_ [charset_ "UTF-8"]
        title_ "Document" -- TODO
        link_ [rel_ "stylesheet", href_ "css/styles.css"]
    body_ $ do
        content

        footer_ [class_ "footer"] $
            div_ [class_ "footer-container"] $
                toHtmlRaw ("&copy; Максим Дикун, 2012 &mdash; 2016<br/>\
                    \Любимый корректор: Анастасия Барбосова" :: Text)
