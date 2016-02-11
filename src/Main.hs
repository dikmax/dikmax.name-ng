
module Main where

import           Config
import           Control.Monad
import qualified Data.Binary                as B
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
import           Rules
import           System.Directory           (createDirectoryIfMissing)
import qualified Template                   as T
import           Text.Pandoc
import           Text.Pandoc.Error          (handleError)
import           Text.Pandoc.Shared
import           Text.Regex.Posix
import           Types


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
    blog

    buildImagesCache

    npmPackages


build :: Rules ()
build =
    phony "build" $ do
        need ["prerequisites"]
        need ["sync-images"]
        need ["images", "blogposts", siteDir </> "css/styles.css"]

blog :: Rules ()
blog = do
    -- Building posts cache
    posts <- newCache $ \file -> do
        need [pandocCacheDir </> file]
        liftIO $ B.decodeFile $ pandocCacheDir </> file :: Action Pandoc

    -- Building images cache
    images <- newCache $ \file -> do
        need [file]
        liftIO $ B.decodeFile file :: Action ImageMeta

    postsList <- newCache $ \t -> do
        postFiles <- getDirectoryFiles "." ["posts//*.md"]
        postCacheContent <- mapM posts postFiles
        return $ buildList t postCacheContent

    phony "blogposts" $ do
        ps <- postsList PostsCacheById
        let postsFilePaths = map (\i -> sitePostsDir </> i </> indexHtml) $ M.keys ps
        let (d,m) = M.size ps `divMod` pageSize
        let listFilePaths = [sitePagesDir </> show p </> indexHtml| p <- [2 .. d + (if m == 0 then 1 else 0)]]
        need $ postsFilePaths ++ [siteDir </> indexHtml] ++ listFilePaths

    sitePostsDir </> "*" </> indexHtml %> \out -> do
        ps <- postsList PostsCacheById
        let post = ps M.! idFromDestFilePath out
        putNormal $ "Writing page " ++ out
        liftIO $ renderToFile out $ T.postPage (getMeta post) $ writeLucid def post

    sitePagesDir </> "*" </> indexHtml %> \out -> do
        ps <- postsList PostsCacheByDate
        let page = (read $ idFromDestFilePath out) :: Int
        let postsOnPage = getPostsForPage ps page
        putNormal $ "Writing page " ++ out
        liftIO $  renderToFile out $ T.listPage $ renderList postsOnPage

    -- Main page
    siteDir </> indexHtml %> \out -> do
        ps <- postsList PostsCacheByDate
        let postsOnPage = getPostsForPage ps 1
        welcome <- posts "index.md"
        putNormal $ "Writing page " ++ out
        liftIO $ renderToFile out $ T.indexPage (getMeta welcome) (writeLucid def welcome) (renderList postsOnPage)

    pandocCacheDir <//> "*.md" %> \out -> do
        let src = dropDirectory2 out
        putNormal $ "Reading post " ++ src
        need [src]
        file <- liftIO $ readFile src
        -- Set "date" from fileName if not present + change field type
        let (Pandoc meta content) = handleError $ readMarkdown readerOptions file
        color <- if isNothing $ coverColor $ getPostCover meta
            then getImageColor images $ coverImg $ getPostCover meta
            else return $ coverColor $ getPostCover meta
        let updatedMeta = Meta $ M.alter (alterDate src) "date"
                               $ M.alter (\_ -> MetaString <$> idFromSrcFilePath src) "id"
                               $ M.insert "cover" (setPostCover $ (getPostCover meta) {coverColor = color})
                               $ unMeta meta
        liftIO $ createDirectoryIfMissing True (takeDirectory out)
        liftIO $ B.encodeFile out (Pandoc updatedMeta content)

    where
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

        getPostsForPage ps page =
            [snd $ M.elemAt i ps | i <- [listLast - rangeEnd .. listLast - rangeStart]]
            where
                listLast = M.size ps - 1
                rangeStart = (page - 1) * pageSize
                rangeEnd = min (page * pageSize - 1) listLast

        alterDate :: FilePath -> Maybe MetaValue -> Maybe MetaValue
        alterDate _ r@(Just (MetaString _)) = r
        alterDate _ (Just (MetaInlines v)) = Just $ MetaString $ concatMap stringify v
        alterDate filePath _ = Just $ MetaString $ dateFromFilePath filePath

        getImageColor :: (FilePath -> Action ImageMeta) -> Maybe String -> Action (Maybe String)
        getImageColor images (Just filePath)
            | "/images/" `isPrefixOf` filePath = do
                meta <- images $ buildDir ++ filePath ++ ".meta"
                return $ Just $ imageColor meta
            | otherwise = return Nothing
        getImageColor _ Nothing = return Nothing

        dateFromFilePath filePath = intercalate "-" $ take 3 $ splitAll "-" $ takeFileName filePath


buildImagesCache :: Rules ()
buildImagesCache =
    imagesBuildDir <//> "*.meta" %> \out -> do
        let src' = imagesDir </> dropDirectory2 out
        let src = take (length src' - 5) src'
        need [src]
        putNormal $ "Reading image " ++ src
        Stdout pixel <- cmd "convert" src "-resize" "1x1!" "txt:-"
        liftIO $ createDirectoryIfMissing True (takeDirectory out)
        liftIO $ B.encodeFile out $ def
            { imageColor = extractColor pixel}
    where
        extractColor p =
            case p =~ pat :: (String, String, String, [String]) of
                (_, _, _, [v]) -> v
                _              -> error $ "Can't extract color from " ++ p
            where
                pat = " (#[0-9A-F]{6})[0-9A-F]{0,2} " -- Could be with opacity, drop it


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
        putNormal "Syncing images"
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

idFromSrcFilePath :: FilePath -> Maybe String
idFromSrcFilePath filePath =
    case filePath =~ pat :: (String, String, String, [String]) of
        (_, _, _, [v]) -> Just v
        _              -> Nothing
    where
        pat = "/[0-9]{4}/[0-9]{4}-[0-9]{2}-[0-9]{2}-(.*)\\.md$"

idFromDestFilePath :: FilePath -> String
idFromDestFilePath filePath =
    case filePath =~ pat :: (String, String, String, [String]) of
        (_, _, _, [v]) -> v
        _              -> error $ "Can't extract id from " ++ filePath
    where
        pat = "/([^/]*)/index.html$"
