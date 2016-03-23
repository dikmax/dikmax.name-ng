module Template.Page.Sitemap (sitemapPage) where

import           BasicPrelude
import           Control.Lens
import           Lucid.Sitemap
import           Types

sitemapPage :: [SitemapUrl] -> Html ()
sitemapPage links =
    sitemapHeader_ $
        forM_ links $ \link ->
            url_ $ do
                loc_ $ toHtml $ link ^. suLoc
                changefreq_ $ toHtml $ link ^. suChangeFreq
                priority_ $ toHtml $ link ^. suPriority
