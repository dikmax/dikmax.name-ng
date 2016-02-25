module Lucid.Atom (
    module Lucid.Atom,
    module Lucid.Base,
    module Lucid.Html5
) where

import           BasicPrelude
import           Lucid.Base
import           Lucid.Html5 (href_, rel_, type_)

atomHeader_ :: Monad m => HtmlT m a -> HtmlT m a
atomHeader_ m = do
    toHtmlRaw ("<?xml version=\"1.0\" encoding=\"UTF-8\" ?>" :: Text)
    rss_ [term "version" "2.0", term "xmlns:atom" "http://www.w3.org/2005/Atom"] m

-- | @atom:link@ element
atomLink_ :: Term arg result => arg -> result
atomLink_ = term "atom:link"

-- | @channel@ element
channel_ :: Term arg result => arg -> result
channel_ = term "channel"

-- | @comments@ element
comments_ :: Term arg result => arg -> result
comments_ = term "comments"

-- | @guid@ element
guid_ :: Term arg result => arg -> result
guid_ = term "guid"

-- | @description@ element
description_ :: Term arg result => arg -> result
description_ = term "description"

-- | @item@ element
item_ :: Term arg result => arg -> result
item_ = term "item"

-- | @language@ element
language_ :: Term arg result => arg -> result
language_ = term "language"

-- | @link@ element
link_ :: Term arg result => arg -> result
link_ = term "link"

-- | @lastBuildDate@ element
lastBuildDate_ :: Term arg result => arg -> result
lastBuildDate_ = term "lastBuildDate"

-- | @pubDate@ element
pubDate_ :: Term arg result => arg -> result
pubDate_ = term "pubDate"

-- | @ttl@ element
ttl_ :: Term arg result => arg -> result
ttl_ = term "ttl"

-- | @rss@ element
rss_ :: Term arg result => arg -> result
rss_ = term "rss"

-- | @rss@ element
title_ :: Term arg result => arg -> result
title_ = term "title"
