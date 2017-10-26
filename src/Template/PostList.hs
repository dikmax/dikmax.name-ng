module Template.PostList (postList) where

import           BasicPrelude
import           Config
import           Control.Lens
import           Lucid
import           Text.Pandoc
import           Text.Pandoc.LucidWriter
import           Text.Pandoc.Utils
import           Types

postList :: CommonData -> Maybe Text -> Maybe Text -> [File] -> Html ()
postList cd olderPage newerPage posts =
    main_ [class_ "main main_no-hero"] $ do
        mconcat $ map (postSingle cd) posts

        when (isJust olderPage || isJust newerPage) $
            div_ [class_ "main__centered pager"] $ do
                maybe (span_ [] mempty) (\link ->
                    a_ [href_ link, class_ "pager__previous"] "← Старше") olderPage
                maybe (span_ [] mempty) (\link ->
                    a_ [href_ link, class_ "pager__next"] "Моложе →") newerPage

postSingle :: CommonData -> File -> Html ()
postSingle cd file =
    div_ [class_ "post post_list"] $ do
        div_ [class_ "main__centered post__block post__title"] $
            a_ [href_ $ postUrlFromId (file ^. fileMeta ^. postId)] $
                toHtml (file ^. fileMeta ^. postTitle)

        maybe mempty (\cover ->
            div_ [class_ "main__centered post__block post__cover"] $
                a_ [href_ $ postUrlFromId (file ^. fileMeta ^. postId)] $
                    img_ [class_ "post__cover-image", src_ cover, alt_ ""]
            ) $ file ^. fileMeta ^. postCover ^. coverImg


        maybe (writeLucid opts $ file ^. fileContent)
            (\(doc, teaser) -> do
                writeLucid opts doc
                div_ [class_ "main__centered post__block post__read-more"] $
                    a_ [href_ $ postUrlFromId (file ^. fileMeta ^. postId)] $ toHtml $
                        if teaser == "" then defaultReadMoreText else teaser
            ) $ extractTeaser $ file ^. fileContent
    where
        opts :: LucidWriterOptions
        opts = def
            & commonData        .~ cd
            & showFigureNumbers .~ False
