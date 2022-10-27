module Template.Page.Service where

import           BasicPrelude
import           Control.Lens
import           Data.Default
import           Lucid
import           Template.Navigation
import           Text.Pandoc.LucidWriter
import           Types


servicePage :: (Html () -> Html ()) -> CommonData -> File -> Html ()
servicePage layout cd service = layout $ do
    navigation False
    main_ [class_ "main main_no-hero service"] $ do
        writeLucid opts $ service ^. fileContent

        h1_ [class_ "main__centered post__block post__block_header-1"] $
            toHtmlRaw ("Контакты" :: Text)
    where
        opts :: LucidWriterOptions
        opts = def
            & commonData        .~ cd
            & showFigureNumbers .~ False
