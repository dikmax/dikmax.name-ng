module Main where

import           Control.Exception
import qualified Data.Binary                as B
import qualified Data.ByteString.Lazy       as LBS
import           Data.Hashable
import           Data.List
import qualified Data.Map.Lazy              as M
import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util
import           Lib
import           LucidWriter
import           System.Directory           (createDirectoryIfMissing)
import           Text.Pandoc
import           Text.Pandoc.Error          (handleError)
import           Text.Pandoc.Shared
import           Text.Regex.Posix

type Posts = M.Map String Pandoc

data PostsCache = PostsCacheById | PostsCacheByDate deriving (Eq)
instance Hashable PostsCache where
    hashWithSalt _ PostsCacheById = 0
    hashWithSalt _ PostsCacheByDate = 1

pageSize :: Int
pageSize = 5

buildDir = "_build"
pandocBuildDir = buildDir </> "pandoc"
shakeBuildDir = buildDir </> "shake"

siteDir = buildDir </> "site"
sitePostsDir = siteDir </> "post"
sitePagesDir = siteDir </> "page"
indexHtml = "index.html"

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
blogPosts = do
    posts <- newCache $ \file -> do
        need [file]
        liftIO $ decodePandocCache file

    postsList <- newCache $ \_ -> do
        postFiles <- getDirectoryFiles "." ["posts//*.md"]
        let cacheFiles = map (\file -> pandocBuildDir </> dropDirectory1 file) postFiles
        postCacheContent <- mapM posts cacheFiles
        let cache = sortBy sortCache postCacheContent
        return $ buildList cache

    phony "blogposts" $ do
        ps <- postsList PostsCacheById
        let postsFilePaths = map (\i -> sitePostsDir </> i </> indexHtml) $ M.keys ps
        let (d,m) = (M.size ps) `divMod` pageSize
        let listFilePaths = [sitePagesDir </> show p </> indexHtml| p <- [2 .. d + (if m == 0 then 1 else 0)]]
        need $ postsFilePaths ++ [siteDir </> indexHtml] ++ listFilePaths
        -- putNormal $ show $ M.keys ps

    sitePostsDir </> "*" </> indexHtml %> \out -> do
        ps <- postsList PostsCacheById
        let post = ps M.! (idFromDestFilePath out)
        putNormal $ "Writing page " ++ out
        liftIO $ writeFile out $ writeLucidString def post

    sitePagesDir </> "*" </> indexHtml %> \out -> do
        ps <- postsList PostsCacheByDate
        let page = (read $ idFromDestFilePath out) :: Int
        let posts = [snd $ M.elemAt i ps | i <- [(page - 1) * pageSize .. min (page * pageSize - 1) (M.size ps - 1)]]
        putNormal $ "Writing page " ++ out
        liftIO $ writeFile out $ show posts

    siteDir </> indexHtml %> \out -> do
        ps <- postsList PostsCacheByDate
        let page = 1
        let posts = [snd $ M.elemAt i ps | i <- [(page - 1) * pageSize .. min (page * pageSize - 1) (M.size ps - 1)]]
        putNormal $ "Writing page " ++ out
        liftIO $ writeFile out $ show posts

    where
        decodePandocCache :: FilePath -> IO Pandoc
        decodePandocCache = B.decodeFile

        buildList :: [Pandoc] -> Posts
        buildList [] = M.empty
        buildList [p] = M.singleton (idFromPost p) p
        buildList (p:ps) =
            let key = idFromPost p;
                listRest = buildList ps in
            case key `M.member` listRest of
                True -> error $ "Duplicate post key " ++ key
                False -> M.insert key p listRest

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
            updatedMeta = Meta $ M.alter (alterDate src) "date"
                               $ M.insert "id" (MetaString $ idFromSrcFilePath src)
                               $ unMeta meta
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

idFromPost :: Pandoc -> String
idFromPost (Pandoc meta _) = maybe (error "Post have no id") getId $ lookupMeta "id" meta
    where
        getId (MetaString s) = s
        getId s = error $ "Post id field have wrong value: " ++ show s

idFromSrcFilePath :: FilePath -> String
idFromSrcFilePath filePath =
    case filePath =~ pat :: (String, String, String, [String]) of
        (_, _, _, [v]) -> v
        otherwise      -> error $ "Can't extract id from " ++ filePath
    where
        pat = "/[0-9]{4}/[0-9]{4}-[0-9]{2}-[0-9]{2}-(.*)\\.md$"

idFromDestFilePath :: FilePath -> String
idFromDestFilePath filePath =
    case filePath =~ pat :: (String, String, String, [String]) of
        (_, _, _, [v]) -> v
        otherwise      -> error $ "Can't extract id from " ++ filePath
    where
        pat = "/([^/]*)/index.html$"
