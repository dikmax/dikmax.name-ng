module Template.Page.List (listPage) where

import           BasicPrelude
import           Lucid
import           Template.Layout
import           Template.Navigation
import           Template.PostList
import           Types


listPage :: CommonData -> FileMeta -> Maybe Text -> Maybe Text -> [File] -> Html ()
listPage cd meta olderPage newerPage posts = layout cd meta $ do
    navigation
    postList olderPage newerPage posts
