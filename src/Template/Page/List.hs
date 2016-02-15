{-# LANGUAGE OverloadedStrings #-}

module Template.Page.List (listPage) where

import           Lucid
import           Template.Layout
import           Template.Navigation
import           Template.PostList
import           Types


listPage :: CommonData -> Maybe String -> Maybe String -> [File] -> Html ()
listPage cd olderPage newerPage posts = layout cd $ do
    navigation
    postList olderPage newerPage posts
