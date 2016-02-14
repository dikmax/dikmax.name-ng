{-# LANGUAGE OverloadedStrings #-}

module Template.PostList (postList) where

import           Config
import           Control.Lens
import           Control.Monad
import           Data.Maybe
import           Data.Text hiding (map)
import           Lucid
import           Text.Pandoc
import           Text.Pandoc.LucidWriter
import           Text.Pandoc.Utils
import           Types

postList :: Maybe String -> Maybe String -> [File] -> Html ()
postList olderPage newerPage posts =
    div_ [class_ "main list-container"] $ do
        mconcat $ map postSingle posts

        when (isJust olderPage || isJust newerPage) $
            div_ [class_ "pager"] $ do
                maybe (span_ [] mempty) (\link ->
                    a_ [href_ $ pack link, class_ "previous"] "← Старше") olderPage
                maybe (span_ [] mempty) (\link ->
                    a_ [href_ $ pack link, class_ "next"] "Моложе →") newerPage

postSingle :: File -> Html ()
postSingle file =
    div_ [class_ "list-post"] $ do
        h1_ [class_ "title"] $
            a_ [href_ $ url (file ^. fileMeta ^. postId)] $ toHtml (file ^. fileMeta ^. postTitle)

        maybe (mempty) (\cover ->
            div_ [class_ "cover"] $
                a_ [href_ $ url (file ^. fileMeta ^. postId)] $
                    img_ [src_ $ pack cover, alt_ ""]
            ) $ file ^. fileMeta ^. postCover ^. coverImg


        maybe (div_ [class_ "description"] $ writeLucid def $ file ^. fileContent) (\(doc, teaser) -> do
            div_ [class_ "description"] $ writeLucid def doc

            div_ [class_ "read-more"] $
                a_ [href_ $ url (file ^. fileMeta ^. postId)] $ toHtml $ if teaser == "" then defaultReadMoreText else teaser
            ) $ extractTeaser $ file ^. fileContent
    where
        url :: String -> Text
        url id' = "/post/" `append` pack id' `append` "/"
