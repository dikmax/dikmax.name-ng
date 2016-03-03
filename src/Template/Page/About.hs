module Template.Page.About where

import           BasicPrelude
import           Control.Lens
import           Data.Default
import           Lucid
import           Template.Navigation
import           Text.Pandoc.LucidWriter
import           Types


aboutPage :: (Html () -> Html ()) -> File -> Html ()
aboutPage layout about = layout $ do
    navigation
    div_ [class_ "main main_list"] $ do
        writeLucid def $ about ^. fileContent
