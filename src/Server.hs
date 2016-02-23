module Server (server) where

import           BasicPrelude
import           FileServe
import           Snap.Core
import           Snap.Http.Server

server :: FilePath -> IO ()
server = quickHttpServe . site

site :: FilePath -> Snap ()
site sitePath =
    path "rss" (redirect "feed.rss") <|>
    path "rss/" (redirect "feed.rss") <|>
    serveDirectory sitePath <|>
    notFoundHandler


notFoundHandler :: Snap ()
notFoundHandler = do
  modifyResponse $ setResponseCode 404
  sendFile "_site/404/index.html"
