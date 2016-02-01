module Main where

import qualified Data.Binary                as B
import qualified Data.ByteString.Lazy       as LBS
import           Data.List
import qualified Data.Map.Lazy              as M
import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util
import           Lib
import           System.Directory           (createDirectoryIfMissing)
import           Text.Pandoc
import           Text.Pandoc.Error          (handleError)

buildDir = "_build"
pandocBuildDir = buildDir </> "pandoc"
shakeBuildDir = buildDir </> "shake"

siteDir = buildDir </> "site"
sitePostsDir = siteDir </> "post"


main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=shakeBuildDir} $ do
    want ["build"]

    phony "clean" $ do
        putNormal "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]

    phony "build" $ do
        need ["blogposts"]

    phony "blogposts" $ do
        postFiles <- getDirectoryFiles "." ["posts//*.md"]
        let cacheFiles = map (\file -> pandocBuildDir </> dropDirectory1 file) postFiles
        need cacheFiles
        postCacheContent <- mapM (\file -> liftIO $ decodePandocCache file) cacheFiles
        let cache = sortBy sortCache postCacheContent
        return ()

    -- Building posts cache
    pandocBuildDir <//> "*.md" %> \out -> do
        let src = "posts" </> dropDirectory1 (dropDirectory1 out)
        need [src]
        putNormal $ "Reading post " ++ src
        file <- liftIO $ readFile src
        -- Adding "file" key to metadata
        let (Pandoc meta content) = handleError $ readMarkdown def file
            updatedMeta = Meta $ M.insert "file" (MetaString src) $ unMeta meta
        liftIO $ createDirectoryIfMissing True (takeDirectory out)
        liftIO $ B.encodeFile out (Pandoc updatedMeta content)

-- TODO
sortCache :: Pandoc -> Pandoc -> Ordering
sortCache (Pandoc meta1 _) (Pandoc meta2 _) = EQ

decodePandocCache :: FilePath -> IO (Pandoc)
decodePandocCache file = B.decodeFile file
