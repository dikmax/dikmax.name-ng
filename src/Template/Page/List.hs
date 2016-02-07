{-# LANGUAGE OverloadedStrings #-}

module Template.Page.List (listPage) where

import Lucid
import Template.Layout
import Template.Navigation
import Template.PostList


listPage :: [Html ()] -> Html ()
listPage posts = layout $ do
    navigation
    postList posts
