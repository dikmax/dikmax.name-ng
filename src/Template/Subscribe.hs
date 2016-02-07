{-# LANGUAGE OverloadedStrings #-}

module Template.Subscribe (subscribe) where

import Lucid

subscribe :: Html ()
subscribe =
    div_ [class_ "subscribe-info"] $
        div_ [class_ "container"] $
            p_ "Подписаться на обновления блога."
