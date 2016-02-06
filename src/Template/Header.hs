{-# LANGUAGE OverloadedStrings #-}

module Template.Header (header) where

import Lucid

header :: Html () -> Html ()
header arg = doctypehtml_ $ do
    head_ $ do
        meta_ [charset_ "UTF-8"]
        title_ "Document" -- TODO
        link_ [rel_ "stylesheet", href_ "css/styles.css"]
    body_ $ arg
