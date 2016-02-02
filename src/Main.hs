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
import           Text.Pandoc.Shared

buildDir = "_build"
pandocBuildDir = buildDir </> "pandoc"
shakeBuildDir = buildDir </> "shake"

siteDir = buildDir </> "site"
sitePostsDir = siteDir </> "post"

options :: ShakeOptions
options = shakeOptions
    { shakeFiles = shakeBuildDir
    , shakeThreads = 0
    , shakeTimings = True
    }

main :: IO ()
main = shakeArgs options $ do
    want ["build"]
    clean
    build
    blogPosts

    buildPostsCache

clean :: Rules ()
clean =
    phony "clean" $ do
        putNormal "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]

build :: Rules ()
build =
    phony "build" $ do
        need ["blogposts"]

blogPosts :: Rules ()
blogPosts =
    phony "blogposts" $ do
        postFiles <- getDirectoryFiles "." ["posts//*.md"]
        let cacheFiles = map (\file -> pandocBuildDir </> dropDirectory1 file) postFiles
        need cacheFiles
        postCacheContent <- mapM (\file -> liftIO $ decodePandocCache file) cacheFiles
        let cache = sortBy sortCache postCacheContent
        putNormal $ show $ map (\(Pandoc m _) -> unMeta m) cache
        return ()
    where
        decodePandocCache :: FilePath -> IO (Pandoc)
        decodePandocCache file = B.decodeFile file

-- Building posts cache
buildPostsCache :: Rules ()
buildPostsCache =
    pandocBuildDir <//> "*.md" %> \out -> do
        let src = "posts" </> dropDirectory1 (dropDirectory1 out)
        need [src]
        putNormal $ "Reading post " ++ src
        file <- liftIO $ readFile src
        -- Set "date" from fileName if not present + change field type
        let (Pandoc meta content) = handleError $ readMarkdown def file
            updatedMeta = Meta $ M.alter (alterDate src) "date" $ unMeta meta
        liftIO $ createDirectoryIfMissing True (takeDirectory out)
        liftIO $ B.encodeFile out (Pandoc updatedMeta content)
    where
        alterDate :: FilePath -> Maybe MetaValue -> Maybe MetaValue
        alterDate _ r@(Just (MetaString _)) = r
        alterDate _ (Just (MetaInlines v)) = Just $ MetaString $ concatMap stringify v
        alterDate filePath _ = Just $ MetaString $ dateFromFilePath filePath

        dateFromFilePath filePath = intercalate "-" $ take 3 $ splitAll "-" $ takeFileName filePath

sortCache :: Pandoc -> Pandoc -> Ordering
sortCache (Pandoc meta1 _) (Pandoc meta2 _) =
    cmp (lookupMeta "date" meta1) (lookupMeta "date" meta2)
    where
        cmp :: Maybe MetaValue -> Maybe MetaValue -> Ordering
        cmp (Just (MetaString _)) Nothing = LT
        cmp Nothing (Just (MetaString _)) = GT
        cmp (Just (MetaString str1)) (Just (MetaString str2)) = compare str2 str1
        cmp _ _ = EQ
