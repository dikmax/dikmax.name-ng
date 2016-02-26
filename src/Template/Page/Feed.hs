module Template.Page.Feed (feedPage) where

import           BasicPrelude
import           Config
import           Control.Lens
import           Data.Default
import           Data.Time
import           Lucid.Atom
import qualified Lucid.Html5             as H
import           Text.Pandoc.LucidWriter
import           Types


feedPage :: UTCTime -> [File] -> Html ()
feedPage now posts = atomHeader_ $
    channel_ mempty $ do
        title_ "[dikmax's blog]"
        description_ "Мой персональный блог"
        link_ "http://dikmax.name/"
        language_ "ru"
        lastBuildDate_ $ toHtml (formatTime defaultTimeLocale rfc822DateFormat now)
        maybe (mempty) (\time ->
            pubDate_ $ toHtml (formatTime defaultTimeLocale rfc822DateFormat time)) $
            (head posts) ^. fileMeta ^?! postDate
        ttl_ "180"
        atomLink_ [href_ "http://dikmax.name/feed.rss", rel_ "self", type_ "application/rss+xml"] mempty

        forM_ posts $ \post -> item_ $ do
            title_ $ toHtml $ post ^. fileMeta ^. postTitle
            description_ $ toHtml $ renderText $ renderSingle post
            link_ $ toHtml $ post ^. fileMeta ^. postUrl
            comments_ $ toHtml $ post ^. fileMeta ^. postUrl ++ "#disqus_thread"
            guid_ $ toHtml $ post ^. fileMeta ^. postUrl
            maybe (mempty) (\time ->
                pubDate_ $ toHtml (formatTime defaultTimeLocale rfc822DateFormat time)) $
                post ^. fileMeta ^?! postDate



renderSingle :: File -> Html ()
renderSingle file = do
    maybe (mempty) (\cover ->
        H.div_ [H.class_ "main__centered post__block post__cover"] $
            H.a_ [H.href_ $ url (file ^. fileMeta ^. postId)] $
                H.img_ [H.class_ "post__cover-image", H.src_ cover, H.alt_ ""]
        ) $ file ^. fileMeta ^. postCover ^. coverImg

    writeLucid opts $ file ^. fileContent

    where
        url :: Text -> Text -- TODO extract to Config
        url id' = "/post/" ++ id' ++ "/"

        opts :: LucidWriterOptions
        opts = def
            { _renderType = RenderRSS
            , _siteDomain = domain
            }
