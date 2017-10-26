module Template.Page.NotFound (notFoundPage) where

import           BasicPrelude
import           Control.Lens
import           Lucid
import           Template.Navigation
import           Text.Pandoc
import           Text.Pandoc.LucidWriter
import           Types

notFoundPage :: (Html () -> Html ()) -> File -> Html ()
notFoundPage layout notFound = layout $ do
    navigation False

    div_ [class_ "main main_no-hero"] $
        main_ [class_ "post"] $ writeLucid def $ notFound ^. fileContent
