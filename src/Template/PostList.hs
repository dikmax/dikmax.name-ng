{-# LANGUAGE OverloadedStrings #-}

module Template.PostList (postList) where

import           Lucid

postList ::[Html ()] -> Html ()
postList posts =
    div_ [class_ "main-contaier"] $ do
        mconcat $ map postSingle posts

        div_ [class_ "pager"] $ do
            a_ [href_ "#", class_ "previous"] "← Старше"
            a_ [href_ "#", class_ "next"] "Моложе →"

postSingle :: Html () -> Html ()
postSingle content =
    div_ [class_ "list-post"] $ do
        h1_ [class_ "title"] $
            a_ [href_ "post.html"] "Осень в Минске"

        div_ [class_ "cover"] $
            img_ [src_ "images/cover.jpg", alt_ ""]

        div_ [class_ "description"] $ content

        div_ [class_ "read-more"] $
            a_ [href_ "#"] "Читать далее..."
