module Lucid.Sitemap (
    module Lucid.Sitemap,
    module Lucid.Base
) where

import           BasicPrelude
import           Lucid.Base

sitemapHeader_ :: Monad m => HtmlT m a -> HtmlT m a
sitemapHeader_ m = do
    toHtmlRaw ("<?xml version=\"1.0\" encoding=\"UTF-8\" ?>" :: Text)
    urlset_ [term "xmlns:atom" "http://www.sitemaps.org/schemas/sitemap/0.9"] m

-- | @changefreq@ element
changefreq_ :: Term arg result => arg -> result
changefreq_ = term "changefreq"

-- | @loc@ element
loc_ :: Term arg result => arg -> result
loc_ = term "loc"

-- | @priority@ element
priority_ :: Term arg result => arg -> result
priority_ = term "priority"

-- | @url@ element
url_ :: Term arg result => arg -> result
url_ = term "url"

-- | @urlset@ element
urlset_ :: Term arg result => arg -> result
urlset_ = term "urlset"

