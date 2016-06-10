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
        liftIO $ do
            createCacheDirectory brotliCacheDir
            createCacheDirectory zopfliCacheDir
            createCacheDirectory webpCacheDir
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
        need $ map (++ ".webp") webpFiles ++
            map (++ ".gz") gzFiles ++
            map (++ ".br") gzFiles

    siteDir <//> "*.jpg.webp" %> \out -> do
        let src = take (length out - 5) out
        process webpCacheDir src out $
            command_ [EchoStdout False, EchoStderr False] "cwebp"
                [src, "-q", "90", "-m", "6", "-o", out]
    siteDir <//> "*.png.webp" %> \out -> do
        let src = take (length out - 5) out
        process webpCacheDir src out $
            command_ [EchoStdout False, EchoStderr False] "cwebp"
                [src, "-lossless", "-q", "100", "-m", "6", "-o", out]
    siteDir <//> "*.gz" %> \out -> do
        let src = take (length out - 3) out
        process zopfliCacheDir src out $
            command_ [] "zopfli" ["--i100", "--gzip", src]
    siteDir <//> "*.br" %> \out -> do
        let src = take (length out - 3) out
        process brotliCacheDir src out $
            command_ [] "bro"
                [ "--input", src, "--output", out
                , "--quality", "10", "--force"]
    where
        process cacheDir src out exec = do
            need [normalizeSrc src]
            file <- liftIO $ BSL.readFile src
            let h = T.unpack $ hashToPath $
                    show (hashlazy file :: Digest SHA3_256)
            exists <- liftIO $ D.doesFileExist $ cacheDir </> h
            if exists
                then liftIO $ D.copyFile (cacheDir </> h) out
                else do
                    () <- exec
                    liftIO $ D.copyFile out (cacheDir </> h)

-- Don't know why but й is represented by file system as two chars.
normalizeSrc :: String -> String
normalizeSrc = T.unpack . T.replace "\1080\774" "\1081" . T.pack

hashToPath :: Text -> Text
hashToPath h = T.take 2 h ++ "/" ++ T.drop 2 h

createCacheDirectory :: FilePath -> IO ()
createCacheDirectory cacheDir = do
    D.createDirectoryIfMissing True cacheDir
    forM_ hex $ \a ->
        forM_ hex $ \b ->
            D.createDirectoryIfMissing False (cacheDir </> [a, b])

    where
        hex = [ '0', '1', '2', '3', '4', '5', '6', '7'
              , '8', '9', 'a', 'b', 'c', 'd', 'e', 'f']
