module Template.Page.Index (indexPage) where

import           BasicPrelude
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

indexPage :: CommonData -> File -> [File] -> Html ()
indexPage cd welcome posts = layout cd (welcome ^. fileMeta) $ do
    div_
        [ class_ "header_for-index dark-background"
        , coverToStyle welcome ] $ writeLucid def $ welcome ^. fileContent

    navigation
    subscribe
    postList (Just "/page/2/") Nothing posts
