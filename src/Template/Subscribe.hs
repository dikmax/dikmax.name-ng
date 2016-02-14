{-# LANGUAGE OverloadedStrings #-}

module Template.Subscribe (subscribe) where

import           Lucid

subscribe :: Html ()
subscribe =
    div_ [class_ "subscribe-info"] $
        div_ [class_ "subscribe-info__centered"] $
            p_ "Подписаться на обновления блога."
