module Template.Navigation (navigation) where

import           BasicPrelude
import           Lucid
import           Template.SvgIcons

navLinks :: [(Text, Text)]
navLinks =
    [ ("/map/", "Путешествия")
    -- , ("/projects/", "Проекты")
    , ("/archive/", "Архив")
    , ("/about/", "Обо мне")
    ]

navigation :: Html ()
navigation = do
    nav_ [class_ "navbar"] $
        div_ [class_ "navbar__container"] $ do
            span_ [class_ "navbar__menu navbar__item"] iconMenu
            span_ [class_ "navbar__brand"] $
                a_ [class_ "navbar__item navbar_link", href_ "/"] "[dikmax's name]"
            span_ [class_ "navbar__item navbar_title hidden"] ":: Осень в Минске"
            span_ [class_ "navbar__navigation"] $
                mapM_ (\(l, t) -> a_ [class_ "navbar__item navbar_link", href_ l] $ toHtml t) navLinks

    nav_ [class_ "sidebar"] $
        div_ [class_ "sidebar__panel"] $ do
            div_ [class_ "sidebar__brand"] "[dikmax's name]"
            nav_ [class_ "sidebar__links"] $
                mapM_ (\(l, t) -> a_ [class_ "sidebar__link", href_ l] $ toHtml t) navLinks
