{-# LANGUAGE OverloadedStrings #-}

module Template.Navigation (navigation) where

import Lucid

navLinks :: Html ()
navLinks = do
    a_ [href_ "/map/"] "Путешествия"
    a_ [href_ "/projects/"] "Проекты"
    a_ [href_ "/archive/"] "Архив"
    a_ [href_ "/about/"] "Обо мне"

navigation :: Html ()
navigation = do
    nav_ [class_ "navbar"] $
        div_ [class_ "navbar-container"] $ do
            span_ [class_ "navbar-brand"] $
                a_ [href_ "/"] "[dikmax's name]"
            span_ [class_ "nav-title hidden"] ":: Осень в Минске"
            span_ [class_ "navbar-nav"] navLinks


    nav_ [class_ "sidebar active"] $
        div_ [class_ "sidebar-panel"] $ do
            div_ [class_ "sidebar-brand"] "[dikmax's name]"
            nav_ [class_ "sidebar-links"] navLinks
