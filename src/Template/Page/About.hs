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
    div_ [class_ "main main_no-hero"] $ do
        writeLucid opts $ about ^. fileContent
    where
        opts :: LucidWriterOptions
        opts = def & showFigureNumbers .~ False
