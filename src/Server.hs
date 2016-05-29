module Server (runServer) where

import           BasicPrelude
import           Config
import           FileServe
import           Snap.Core
import           Snap.Http.Server
import           System.Directory           (createDirectoryIfMissing, doesFileExist)

runServer :: IO ()
runServer = do
    createDirectoryIfMissing True "log"
    accessLog <- doesFileExist "log/access.log"
    unless accessLog $ writeFile "log/access.log" ""
    errorLog <- doesFileExist "log/error.log"
    unless errorLog $ writeFile "log/error.log" ""

    server siteDir

server :: FilePath -> IO ()
server = quickHttpServe . site

site :: FilePath -> Snap ()
site sitePath =
    path "rss" (redirect "feed.rss") <|>
    path "rss/" (redirect "feed.rss") <|>
    serveDirectory sitePath <|>
    notFoundHandler sitePath


notFoundHandler :: FilePath -> Snap ()
notFoundHandler sitePath = do
  modifyResponse $ setResponseCode 404
  sendFile $ sitePath ++ "/404/index.html"
