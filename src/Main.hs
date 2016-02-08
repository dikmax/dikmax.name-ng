
module Main where

import           Control.Exception
import           Control.Monad
import qualified Data.Binary                as B
import qualified Data.ByteString.Lazy       as LBS
import           Data.Hashable
import           Data.List
import qualified Data.Map.Lazy              as M
import           Data.Maybe
import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.Config
import           Development.Shake.FilePath
import           Development.Shake.Util
import           Lib
import           Lucid
import           LucidWriter
import           Server
import           System.Directory           (createDirectoryIfMissing)
import           System.Exit
import qualified Template                   as T
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

nodeModulesDir = "node_modules"
nodeModulesBinDir = nodeModulesDir </> ".bin"

imagesDir = "images"

postcss = nodeModulesBinDir </> "postcss"

options :: ShakeOptions
options = shakeOptions
    { shakeFiles    = shakeBuildDir
    , shakeThreads  = 0
    , shakeTimings  = True
    , shakeProgress = progressSimple
    }

readerOptions :: ReaderOptions
readerOptions = def
    { readerSmart = True
    , readerParseRaw = True
    }

main :: IO ()
main = shakeArgs options $ do
    usingConfigFile "build.cfg"
    want ["build"]
    clean
    runServer
    prerequisites
    build
    styles
    images
    blogPosts

    buildPostsCache

    npmPackages

clean :: Rules ()
clean =
    phony "clean" $ do
        putNormal "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]

runServer :: Rules ()
runServer =
    phony "server" $ do
        liftIO $ createDirectoryIfMissing True "log"
        accessLog <- doesFileExist "log/access.log"
        unless accessLog $ liftIO $ writeFile "log/access.log" ""
        errorLog <- doesFileExist "log/error.log"
        unless errorLog $ liftIO $ writeFile "log/error.log" ""

        liftIO $ server siteDir

prerequisites :: Rules ()
prerequisites =
    phony "prerequisites" $ do
        putNormal "Checking prerequisites"
        check "node"
        check "npm"
        check "rsync"
    where
        check command = do
            Exit code <- cmd (EchoStdout False) "which" command
            when (code /= ExitSuccess) $ error $ "PREREQUISITE: '" ++ command ++ "' is not available"


build :: Rules ()
build =
    phony "build" $ do
        need ["prerequisites"]
        need ["sync-images"]
        need ["images", "blogposts", siteDir </> "css/styles.css"]

blogPosts :: Rules ()
blogPosts = do
    -- Builing posts cache
    posts <- newCache $ \file -> do
        need [file]
        liftIO $ decodePandocCache file

    postsList <- newCache $ \t -> do
        postFiles <- getDirectoryFiles "." ["posts//*.md"]
        let cacheFiles = map (\file -> pandocBuildDir </> dropDirectory1 file) postFiles
        postCacheContent <- mapM posts cacheFiles
        return $ buildList t postCacheContent

    phony "blogposts" $ do
        ps <- postsList PostsCacheById
        let postsFilePaths = map (\i -> sitePostsDir </> i </> indexHtml) $ M.keys ps
        let (d,m) = M.size ps `divMod` pageSize
        let listFilePaths = [sitePagesDir </> show p </> indexHtml| p <- [2 .. d + (if m == 0 then 1 else 0)]]
        need $ postsFilePaths ++ [siteDir </> indexHtml] ++ listFilePaths
        -- putNormal $ show $ M.keys ps

    sitePostsDir </> "*" </> indexHtml %> \out -> do
        ps <- postsList PostsCacheById
        let post = ps M.! idFromDestFilePath out
        putNormal $ "Writing page " ++ out
        liftIO $ renderToFile out $ T.postPage (getMeta post) $ writeLucid def post

    sitePagesDir </> "*" </> indexHtml %> \out -> do
        ps <- postsList PostsCacheByDate
        let page = (read $ idFromDestFilePath out) :: Int
        let posts = postsPage ps page
        putNormal $ "Writing page " ++ out
        liftIO $  renderToFile out $ T.listPage $ renderList posts

    siteDir </> indexHtml %> \out -> do
        ps <- postsList PostsCacheByDate
        let posts = postsPage ps 1
        putNormal $ "Writing page " ++ out
        liftIO $ renderToFile out $ T.indexPage $ renderList posts

    where
        decodePandocCache :: FilePath -> IO Pandoc
        decodePandocCache = B.decodeFile

        buildList :: PostsCache -> [Pandoc] -> Posts
        buildList _ [] = M.empty
        buildList PostsCacheById [p] = M.singleton (idFromPost p) p
        buildList PostsCacheById (p:ps) =
            let key = idFromPost p;
                listRest = buildList PostsCacheById ps in
            if key `M.member` listRest
                then error $ "Duplicate post key " ++ key
                else M.insert key p listRest
        buildList PostsCacheByDate [p] = M.singleton (dateFromPost p) p
        buildList PostsCacheByDate (p:ps) =
            let key = dateFromPost p;
                listRest = buildList PostsCacheByDate ps in
            if key `M.member` listRest
                then error $ "Duplicate post key " ++ key
                else M.insert key p listRest

        renderList :: [Pandoc] -> [Html ()]
        renderList = map (writeLucid def)

        postsPage ps page =
            [snd $ M.elemAt i ps | i <- [listLast - rangeEnd .. listLast - rangeStart]]
            where
                listLast = M.size ps - 1
                rangeStart = (page - 1) * pageSize
                rangeEnd = min (page * pageSize - 1) listLast


-- Building posts cache
buildPostsCache :: Rules ()
buildPostsCache =
    pandocBuildDir <//> "*.md" %> \out -> do
        let src = "posts" </> dropDirectory2 out
        need [src]
        putNormal $ "Reading post " ++ src
        file <- liftIO $ readFile src
        -- Set "date" from fileName if not present + change field type
        let (Pandoc meta content) = handleError $ readMarkdown readerOptions file
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


-- Build styles
styles :: Rules ()
styles =
    siteDir </> "css/*.css" %> \out -> do
        let src = "styles" </> dropDirectory3 out -<.> "pcss"
        files <- getDirectoryFiles "." ["styles//*"]
        need (postcss : "postcss.json" : files)
        cmd (FileStdout out) postcss "-c" "postcss.json" src


-- Build images
images :: Rules ()
images = do
    phony "sync-images" $ do
        srcImagesDir <- getConfig "IMAGES_DIR"
        when (isJust srcImagesDir) $ do
            let dir = fromMaybe "" srcImagesDir
            files <- getDirectoryFiles dir ["//*"]
            -- TODO delete no more existent files
            forM_ files (\file -> do
                exists <- doesFileExist (imagesDir </> file)
                unless exists $ do
                    liftIO $ createDirectoryIfMissing True $ takeDirectory (imagesDir </> file)
                    putNormal $ "Copying file " ++ (imagesDir </> file)
                    copyFileChanged (dir </> file) (imagesDir </> file)
                )

    phony "images" $ do
        imageFiles <- getDirectoryFiles "." imagesPatterns
        need [siteDir </> x | x <- imageFiles]

    forM_ imagesPatterns buildStatic

    where
        imagesPatterns :: [FilePath]
        imagesPatterns = [imagesDir <//> "*.png", imagesDir <//> "*.jpg", imagesDir <//> "*.gif"]


-- npm packages
npmPackages :: Rules ()
npmPackages =
    [postcss] &%> \_ -> do
        need ["package.json"]
        cmd "npm" "install"

-- Static files, that just should be copied to `siteDir`
buildStatic :: FilePath -> Rules ()
buildStatic filePath =
    siteDir </> filePath %> \out -> do
        let src = dropDirectory2 out
        putNormal $ "Copying file " ++ out
        copyFileChanged src out


idFromPost :: Pandoc -> String
idFromPost (Pandoc meta _) = maybe (error "Post have no id") getId $ lookupMeta "id" meta
    where
        getId (MetaString s) = s
        getId s = error $ "Post id field have wrong value: " ++ show s

dateFromPost :: Pandoc -> String
dateFromPost (Pandoc meta _) = maybe (error "Post have no date") getDate $ lookupMeta "date" meta
    where
        getDate (MetaString s) = s
        getDate s = error $ "Post date field have wrong value: " ++ show s

idFromSrcFilePath :: FilePath -> String
idFromSrcFilePath filePath =
    case filePath =~ pat :: (String, String, String, [String]) of
        (_, _, _, [v]) -> v
        _              -> error $ "Can't extract id from " ++ filePath
    where
        pat = "/[0-9]{4}/[0-9]{4}-[0-9]{2}-[0-9]{2}-(.*)\\.md$"

idFromDestFilePath :: FilePath -> String
idFromDestFilePath filePath =
    case filePath =~ pat :: (String, String, String, [String]) of
        (_, _, _, [v]) -> v
        _              -> error $ "Can't extract id from " ++ filePath
    where
        pat = "/([^/]*)/index.html$"
