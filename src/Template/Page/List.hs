{-# LANGUAGE OverloadedStrings #-}

module Template.Page.List (listPage) where

import           Lucid
import           Template.Layout
import           Template.Navigation
import           Template.PostList
import           Types


listPage :: CommonData -> FileMeta -> Maybe String -> Maybe String -> [File] -> Html ()
listPage cd meta olderPage newerPage posts = layout cd meta $ do
    navigation
    postList olderPage newerPage posts
