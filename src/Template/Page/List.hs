module Template.Page.List (listPage) where

import           BasicPrelude
import           Lucid
import           Template.Navigation
import           Template.PostList
import           Types


listPage :: (Html () -> Html ()) -> Maybe Text -> Maybe Text -> [File] -> Html ()
listPage layout olderPage newerPage posts = layout $ do
    navigation
    postList olderPage newerPage posts
