{-# LANGUAGE OverloadedStrings #-}

module Template.Page.Index (indexPage) where

import Lucid
import Template.Layout
import Template.Navigation
import Template.PostList
import Template.Subscribe

indexPage :: [Html ()] -> Html ()
indexPage posts = layout $ do
    div_ [class_ "main-page-header dark-background"] $
        div_ [class_ "container"] "TODO header not implemented"

    navigation
    subscribe
    postList posts
