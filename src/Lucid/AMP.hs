module Lucid.AMP where

import           BasicPrelude
import           Lucid
import           Lucid.Base

-- | Amp html
ampDoctypeHtml_ :: Monad m => HtmlT m a -> HtmlT m a
ampDoctypeHtml_ m = do
    toHtmlRaw ("<!doctype html>" :: Text)
    html_ [term "⚡" ""] m

ampBoilerplate_ :: Monad m => HtmlT m ()
ampBoilerplate_ = do
    toHtmlRaw ("<style amp-boilerplate>body{-webkit-animation:-amp-start 8s steps(1,end) 0s 1 normal both;\
        \-moz-animation:-amp-start 8s steps(1,end) 0s 1 normal both;-ms-animation:\
        \-amp-start 8s steps(1,end) 0s 1 normal both;animation:-amp-start 8s steps(1,end) 0s 1 normal both}\
        \@-webkit-keyframes -amp-start{from{visibility:hidden}to{visibility:visible}}\
        \@-moz-keyframes -amp-start{from{visibility:hidden}to{visibility:visible}}\
        \@-ms-keyframes -amp-start{from{visibility:hidden}to{visibility:visible}}\
        \@-o-keyframes -amp-start{from{visibility:hidden}to{visibility:visible}}\
        \@keyframes -amp-start{from{visibility:hidden}to{visibility:visible}}</style>\
        \<noscript><style amp-boilerplate>body{-webkit-animation:none;-moz-animation:none;\
        \-ms-animation:none;animation:none}</style></noscript>" :: Text)
    script_ [async_ "", src_ "https://cdn.ampproject.org/v0.js"] ("" :: Text)

-- | @amp-analytics@ element
ampAnalytics_ :: Term arg result => arg -> result
ampAnalytics_ = term "amp-analytics"

-- | @amp-iframe@ element
ampIframe_ :: Term arg result => arg -> result
ampIframe_ = term "amp-iframe"

-- | @amp-img@ element
ampImg_ :: Monad m => [Attribute] -> HtmlT m ()
ampImg_ = with (makeElementNoEnd "amp-img")

-- | @amp-sidebar@ element
ampSidebar_ :: Term arg result => arg -> result
ampSidebar_ = term "amp-sidebar"

-- | @amp-youtube@ element
ampYoutube_ :: Term arg result => arg -> result
ampYoutube_ = term "amp-youtube"
