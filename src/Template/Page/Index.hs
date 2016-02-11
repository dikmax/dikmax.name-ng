{-# LANGUAGE OverloadedStrings #-}

module Template.Page.Index (indexPage) where

import           Lib
import           Lucid
import           Template.Common
import           Template.Layout
import           Template.Navigation
import           Template.PostList
import           Template.Subscribe
import           Text.Pandoc

indexPage :: Meta -> Html () -> [Html ()] -> Html ()
indexPage meta welcome posts = layout $ do
    div_
        [ class_ "main-page-header dark-background"
        , coverToStyle $ getPostCover meta ] $
        div_ [class_ "container"] welcome

    navigation
    subscribe
    postList posts
