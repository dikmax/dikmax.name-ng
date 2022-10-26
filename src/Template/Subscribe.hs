module Template.Subscribe (subscribe) where

import           BasicPrelude
import           Lucid

subscribe :: Html ()
subscribe =
    div_ [class_ "subscribe-info"] $
        div_ [class_ "subscribe-info__centered"] $
            toHtmlRaw ("<style type=\"text/css\">\
                       \#mc_embed_signup{clear:left;width:100%;font-family:Roboto,sans-serif}\
                       \#mc_embed_signup form {text-align:center;padding:10px 0}\
                       \.mc-field-group{display:inline-block}\
                       \#mc_embed_signup input.email{font-size:15px;border: 1px solid #ABB0B2;border-radius:3px;color:#343434;background-color:#fff;box-sizing:border-box;height:32px;padding:0 0.4em;display:inline-block;margin:0;width:350px;vertical-align:top}\
                       \#mc_embed_signup label{display:block;font-size:16px;padding-bottom:10px;font-weight:500}\
                       \#mc_embed_signup .clear{position:relative;top:-2px;display:inline-block}\
                       \#mc_embed_signup .button{font-size:13px;border:none;border-radius:3px;letter-spacing:.03em;color:#fff;background-color:#aaa;box-sizing:border-box;height:32px;line-height:32px;padding:0 18px;display:inline-block;margin:0;transition:all 0.23s ease-in-out 0s}\
                       \#mc_embed_signup .button:hover{background-color:#777;cursor:pointer}\
                       \#mc_embed_signup div#mce-responses{float:left;top:-1.4em;padding:0 .5em;overflow:hidden;width:90%;margin:0 5%;clear:both}\
                       \#mc_embed_signup div.response{margin:1em 0;padding:1em .5em .5em 0;font-weight:bold;float:left;top:-1.5em;z-index:1;width:80%}\
                       \#mc_embed_signup #mce-error-response{display:none}\
                       \#mc_embed_signup #mce-success-response{color:#529214;display:none}\
                       \#mc_embed_signup label.error{display:block;float:none;width:auto;margin-left:1.05em;text-align:left;padding:.5em 0}\
                       \@media(max-width:768px){\
                           \#mc_embed_signup input.email{width:100%;margin-bottom:5px}\
                           \#mc_embed_signup .clear{display:block;width: 100%}\
                           \#mc_embed_signup .button{width:100%;margin:0}\
                       \}\
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
