module Template.Navigation (navigation) where

import           BasicPrelude
import           Lucid
import           Lucid.AMP
import           Template.SvgIcons

navLinks :: [(Text, Text)]
navLinks =
    [ ("/map/", "Путешествия")
    -- , ("/projects/", "Проекты")
    , ("/archive/", "Архив")
    , ("/about/", "Обо мне")
    ]

navigation :: Bool -> Html ()
navigation isAmp = do
    nav_ [class_ "navbar"] $
        div_ [class_ "navbar__container"] $ do
            if isAmp
            then
                span_ [class_ "navbar__menu navbar__item", role_ "button",
                    tabindex_ "0", term "on" "tap:sidebar.toggle"] iconMenu
            else
                span_ [class_ "navbar__menu navbar__item"] iconMenu
            span_ [class_ "navbar__brand"] $
                a_ [class_ "navbar__item navbar_link", href_ "/"] "[dikmax's blog]"
            span_ [class_ "navbar__item navbar_title hidden"] ":: Осень в Минске"
            span_ [class_ "navbar__navigation"] $
                mapM_ (\(l, t) -> a_ [class_ "navbar__item navbar_link", href_ l] $ toHtml t) navLinks

    if isAmp
    then
        ampSidebar_ [id_ "sidebar", class_ "sidebar", term "layout" "nodisplay"] $
            div_ [class_ "sidebar__panel sidebar__panel_active sidebar__panel_amp"] $ do
                div_ [class_ "sidebar__brand", role_ "button",
                    tabindex_ "0", term "on" "tap:sidebar.close"] "[dikmax's blog]"
                nav_ [class_ "sidebar__links"] $
                    mapM_ (\(l, t) -> a_ [class_ "sidebar__link", href_ l] $ toHtml t) navLinks
    else
        nav_ [class_ "sidebar"] $
            div_ [class_ "sidebar__panel"] $ do
                div_ [class_ "sidebar__brand"] "[dikmax's blog]"
                nav_ [class_ "sidebar__links"] $
                    mapM_ (\(l, t) -> a_ [class_ "sidebar__link", href_ l] $ toHtml t) navLinks
