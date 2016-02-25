module Lucid.AMP where

import           BasicPrelude
import           Lucid
import           Lucid.Base

-- | Amp html
ampDoctypeHtml_ :: Monad m => HtmlT m a -> HtmlT m a
ampDoctypeHtml_ m = do
    doctype_
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

-- | @amp-img@ element
ampImg_ :: Monad m => [Attribute] -> HtmlT m ()
ampImg_ = with (makeElementNoEnd "amp-img")
