{-# LANGUAGE OverloadedStrings #-}

module Template.Page.List (listPage) where

import           Lucid
import           Template.Layout
import           Template.Navigation
import           Template.PostList
import           Types


listPage :: Maybe String -> Maybe String -> [File] -> Html ()
listPage olderPage newerPage posts = layout $ do
    navigation
    postList olderPage newerPage posts
