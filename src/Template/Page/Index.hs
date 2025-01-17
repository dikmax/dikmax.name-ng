module Template.Page.Index (indexPage) where

import           BasicPrelude
import           Control.Lens
import           Lucid
import           Template.Common
import           Template.Navigation
import           Template.PostList
import           Template.Subscribe
import           Text.Pandoc
import           Text.Pandoc.LucidWriter
import           Types

indexPage :: (Html () -> Html ())
          -> CommonData
          -> File
          -> [File]
          -> Html ()
indexPage layout cd welcome posts = layout $ do
    div_
        [ class_ "header_for-index dark-background"
        , style_ $ coverToStyle welcome ] $ writeLucid def $ welcome ^. fileContent

    navigation False
    subscribe
    postList cd (Just "/page/2/") Nothing posts
