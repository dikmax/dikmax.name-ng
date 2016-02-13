{-# LANGUAGE OverloadedStrings #-}

module Template.PostList (postList) where

import           Control.Lens
import           Lucid
import           Text.Pandoc
import           Text.Pandoc.LucidWriter
import           Types

postList :: [File] -> Html ()
postList posts =
    div_ [class_ "main-container"] $ do
        mconcat $ map postSingle posts

        div_ [class_ "pager"] $ do
            a_ [href_ "#", class_ "previous"] "← Старше"
            a_ [href_ "#", class_ "next"] "Моложе →"

postSingle :: File -> Html ()
postSingle file =
    div_ [class_ "list-post"] $ do
        h1_ [class_ "title"] $
            a_ [href_ "post.html"] "Осень в Минске"

        div_ [class_ "cover"] $
            img_ [src_ "images/cover.jpg", alt_ ""]

        div_ [class_ "description"] $ writeLucid def $ file ^. fileContent

        div_ [class_ "read-more"] $
            a_ [href_ "#"] "Читать далее..."
