module Template.Page.About where

import           BasicPrelude
import           Control.Lens
import           Data.Default
import           Lucid
import           Template.Navigation
import           Template.SvgIcons
import           Text.Pandoc.LucidWriter
import           Types

buttons :: [(Text, Text, Text, Html ())]
buttons =
    [ ("E-mail",   "email",       "mailto:me@dikmax.name",                               iconEmail)
    , ("Facebook", "facebook",    "https://www.facebook.com/dikmax",                     iconFacebook)
    , ("Google+",  "google-plus", "https://www.google.com/+MaximDikun",                  iconGooglePlus)
    , ("LinkedIn", "linkedin",    "https://www.linkedin.com/pub/maxim-dikun/36/389/303", iconLinkedIn)
    , ("VK",       "vk",          "https://vk.com/dikmax",                               iconVk)
    , ("last.fm",  "lastfm",      "http://www.last.fm/user/dikmax",                      iconLastfm)
    , ("Instagram","instagram",   "https://www.instagram.com/dikmax/",                   iconInstagram)
    , ("GitHub",   "github",      "https://github.com/dikmax",                           iconGitHub)
    , ("StackOverflow","stack-overflow","https://stackoverflow.com/users/682727/dikmax", iconStackOverflow)
    , ("Twitter",  "twitter",     "https://twitter.com/dikmax",                          iconTwitter)
    , ("YouTube",  "youtube",     "https://www.youtube.com/user/zabeydikmax",            iconYouTube)
    ]


aboutPage :: (Html () -> Html ()) -> File -> Html ()
aboutPage layout about = layout $ do
    navigation
    div_ [class_ "main main_no-hero"] $ do
        writeLucid opts $ about ^. fileContent

        h1_ [class_ "main__centered post__block post__block_header-1"] $
            toHtmlRaw ("Контакты" :: Text)
        div_ [class_ "main__centered post__block post__block_div about__contacts"] $
            forM_ buttons $ \(title, cl, link, icon) ->
                a_ [ href_ link
                   , title_ title
                   , class_ $ "about__contact about__contact_" ++ cl] icon
    where
        opts :: LucidWriterOptions
        opts = def & showFigureNumbers .~ False
