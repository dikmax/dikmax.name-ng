module Template.Subscribe (subscribe) where

import           BasicPrelude
import           Lucid

subscribe :: Html ()
subscribe =
    div_ [class_ "subscribe-info"] $
        div_ [class_ "subscribe-info__centered"] $
            toHtmlRaw ("<link href=\"//cdn-images.mailchimp.com/embedcode/horizontal-slim-10_7.css\" rel=\"stylesheet\" type=\"text/css\">\
                       \<style type=\"text/css\">\
                       \#mc_embed_signup{clear:left;width:100%;}\
                       \#mc_embed_signup .clear{position:relative;top:-2px;}\
                       \</style>\
                       \<div id=\"mc_embed_signup\">\
                       \<form action=\"//dikmax.us10.list-manage.com/subscribe/post?u=5d08ed74a8faa5d54bebd5796&amp;id=4455dc1a2a\" method=\"post\" id=\"mc-embedded-subscribe-form\" name=\"mc-embedded-subscribe-form\" class=\"validate\" target=\"_blank\" novalidate>\
                           \<div id=\"mc_embed_signup_scroll\">\
                       \<label for=\"mce-EMAIL\">Подписаться на обновления блога</label>\
                       \<input type=\"email\" value=\"\" name=\"EMAIL\" class=\"email\" id=\"mce-EMAIL\" placeholder=\"email\" required>\
                           \<div style=\"position: absolute; left: -5000px;\" aria-hidden=\"true\"><input type=\"text\" name=\"b_5d08ed74a8faa5d54bebd5796_4455dc1a2a\" tabindex=\"-1\" value=\"\"></div>\
                           \<div class=\"clear\"><input type=\"submit\" value=\"Подписаться\" name=\"subscribe\" id=\"mc-embedded-subscribe\" class=\"button\"></div>\
                           \</div>\
                       \</form>\
                       \</div>" :: Text)

-- http://eepurl.com/bjyAm5
