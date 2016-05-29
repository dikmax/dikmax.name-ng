module Template.Page.List (listPage) where

import           BasicPrelude
import           Lucid
import           Template.Navigation
import           Template.PostList
import           Types


listPage :: (Html () -> Html ())
         -> CommonData
         -> Maybe Text
         -> Maybe Text
         -> [File]
         -> Html ()
listPage layout cd olderPage newerPage posts = layout $ do
    navigation False
    postList cd olderPage newerPage posts
