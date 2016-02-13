{-# LANGUAGE OverloadedStrings #-}

module Template.Page.Index (indexPage) where

import           Control.Lens
import           Lucid
import           Template.Common
import           Template.Layout
import           Template.Navigation
import           Template.PostList
import           Template.Subscribe
import           Text.Pandoc
import           Text.Pandoc.LucidWriter
import           Types

indexPage :: File -> [File] -> Html ()
indexPage welcome posts = layout $ do
    div_
        [ class_ "main-page-header dark-background"
        , coverToStyle welcome ] $
        div_ [class_ "container"] $ writeLucid def $ welcome ^. fileContent

    navigation
    subscribe
    postList (Just "/page/2/") Nothing posts
