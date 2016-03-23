module Compress where

import           BasicPrelude
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text            as T
import           Config
import           Crypto.Hash
import           Development.Shake
import qualified System.Directory     as D


compress :: Rules ()
compress = do
    phony "compress" $ do
        liftIO $ D.createDirectoryIfMissing True zopfliCacheDir
        liftIO $ D.createDirectoryIfMissing True webpCacheDir
        webpFiles <- getDirectoryFiles "."
            [ siteDir <//> "*.jpg"
            , siteDir <//> "*.png"
            ]
        gzFiles <- getDirectoryFiles "."
            [ siteDir <//> "*.css"
            , siteDir <//> "*.js"
            , siteDir <//> "*.json"
            , siteDir <//> "*.html"
            , siteDir <//> "*.rss"
            , siteDir <//> "*.txt"
            , siteDir <//> "*.xml"
            ]
        need $ (map (++ ".webp") webpFiles) ++ (map (++ ".gz") gzFiles)

    siteDir <//> "*.jpg.webp" %> \out -> do
        let src = take (length out - 5) out
        process webpCacheDir src out $
            command_ [EchoStdout False, EchoStderr False] "cwebp"
                [src, "-jpeg_like", "-mt", "-m", "6", "-o", out]
    siteDir <//> "*.png.webp" %> \out -> do
        let src = take (length out - 5) out
        process webpCacheDir src out $
            command_ [EchoStdout False, EchoStderr False] "cwebp"
                [src, "-mt", "-lossless", "-q", "100", "-m", "6", "-o", out]
    siteDir <//> "*.gz" %> \out -> do
        let src = take (length out - 3) out
        process zopfliCacheDir src out $
            command_ [] "zopfli" ["--i100", "--gzip", src]
    where
        process cacheDir src out exec = do
            need [normalizeSrc src]
            file <- liftIO $ BSL.readFile src
            let h = T.unpack $ show (hashlazy file :: Digest SHA3_256)
            exists <- liftIO $ D.doesFileExist $ cacheDir </> h
            if (exists)
                then do
                    liftIO $ D.copyFile (cacheDir </> h) out
                else do
                    () <- exec
                    liftIO $ D.copyFile out (cacheDir </> h)

-- Don't know why but й is represented by file system as two chars.
normalizeSrc :: String -> String
normalizeSrc = T.unpack . T.replace "\1080\774" "\1081" . T.pack


