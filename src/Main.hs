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
        -- Adding "file" key to metadata
        let (Pandoc meta content) = handleError $ readMarkdown def file
            updatedMeta = Meta $ M.insert "file" (MetaString src) $ unMeta meta
        liftIO $ createDirectoryIfMissing True (takeDirectory out)
        liftIO $ B.encodeFile out (Pandoc updatedMeta content)
        -- TODO use M.alter to add date if it's not present in metadata

-- TODO
sortCache :: Pandoc -> Pandoc -> Ordering
sortCache (Pandoc meta1 _) (Pandoc meta2 _) =
    -- TODO use docDate
    cmp (lookupMeta "date" meta1) (lookupMeta "date" meta2)
    where
        cmp :: Maybe MetaValue -> Maybe MetaValue -> Ordering
        cmp (Just (MetaString _)) Nothing = GT
        cmp Nothing (Just (MetaString _)) = LT
        cmp (Just (MetaString str1)) (Just (MetaString str2)) = compare str1 str2
        cmp _ _ = EQ
