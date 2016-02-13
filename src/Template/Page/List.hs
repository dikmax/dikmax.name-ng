{-# LANGUAGE OverloadedStrings #-}

module Template.Page.List (listPage) where

import           Lucid
import           Template.Layout
import           Template.Navigation
import           Template.PostList
import           Types


listPage :: [File] -> Html ()
listPage posts = layout $ do
    navigation
    postList posts
