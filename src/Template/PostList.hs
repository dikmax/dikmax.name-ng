module Template.PostList (postList) where

import           BasicPrelude
import           Config
import           Control.Lens
import           Lucid
import           Text.Pandoc
import           Text.Pandoc.LucidWriter
import           Text.Pandoc.Utils
import           Types

postList :: Maybe Text -> Maybe Text -> [File] -> Html ()
postList olderPage newerPage posts =
    div_ [class_ "main main_no-hero"] $ do
        mconcat $ map postSingle posts

        when (isJust olderPage || isJust newerPage) $
            div_ [class_ "main__centered pager"] $ do
                maybe (span_ [] mempty) (\link ->
                    a_ [href_ link, class_ "pager__previous"] "← Старше") olderPage
                maybe (span_ [] mempty) (\link ->
                    a_ [href_ link, class_ "pager__next"] "Моложе →") newerPage

postSingle :: File -> Html ()
postSingle file =
    div_ [class_ "post post_list"] $ do
        div_ [class_ "main__centered post__block post__title"] $
            a_ [href_ $ url (file ^. fileMeta ^. postId)] $ toHtml (file ^. fileMeta ^. postTitle)

        maybe mempty (\cover ->
            div_ [class_ "main__centered post__block post__cover"] $
                a_ [href_ $ url (file ^. fileMeta ^. postId)] $
                    img_ [class_ "post__cover-image", src_ cover, alt_ ""]
            ) $ file ^. fileMeta ^. postCover ^. coverImg


        maybe (writeLucid def $ file ^. fileContent)
            (\(doc, teaser) -> do
                writeLucid opts doc
                div_ [class_ "main__centered post__block post__read-more"] $
                    a_ [href_ $ url (file ^. fileMeta ^. postId)] $ toHtml $
                        if teaser == "" then defaultReadMoreText else teaser
            ) $ extractTeaser $ file ^. fileContent
    where
        url :: Text -> Text -- TODO extract to Config
        url id' = "/post/" ++ id' ++ "/"

        opts :: LucidWriterOptions
        opts = def & showFigureNumbers .~ False
