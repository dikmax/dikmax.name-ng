
module Main where

import           Config
import           Control.Monad
import           Control.Lens
import qualified Data.Binary                as B
import           Data.List
import qualified Data.Map.Lazy              as M
import           Data.Maybe
import           Data.Time
import           Development.Shake
import           Development.Shake.Config
import           Development.Shake.FilePath
import           Images
import           Lib
import           Lucid
import           Rules
import           System.Directory           (createDirectoryIfMissing)
import qualified Template                   as T
import           Text.Pandoc
import           Text.Pandoc.Error          (handleError)
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
    imagesRules
    blog

    buildImagesCache

    npmPackages


build :: Rules ()
build =
    phony "build" $ do
        need ["prerequisites"]
        need ["sync-images"]
        need ["images", "blogposts"]

blog :: Rules ()
blog = do
    -- Building posts cache
    posts <- newCache $ \file -> do
        need [pandocCacheDir </> file]
        liftIO $ B.decodeFile $ pandocCacheDir </> file :: Action File

    -- Building images cache
    image <- newCache $ \file -> do
        need [file]
        liftIO $ B.decodeFile file :: Action ImageMeta

    images <- newCache $ \_ -> do
        imageFiles <- getDirectoryFiles "." imagesPatterns
        need imageFiles
        imageCacheContent <- mapM (\i -> do -- TODO parallel
            meta <- image $ buildDir </> i ++ ".meta"
            return (i, meta)) imageFiles
        return $ M.fromList imageCacheContent

    postsList <- newCache $ \t -> do
        postFiles <- getDirectoryFiles "." ["posts//*.md"]
        -- TODO make parallel
        postCacheContent <- mapM posts postFiles
        return $ buildList t postCacheContent

    tagsList <- newCache $ \_ -> do
        postFiles <- getDirectoryFiles "." ["posts//*.md"]
        -- TODO make parallel
        postCacheContent <- mapM posts postFiles
        return $ buildTags postCacheContent

    css <- newCache $ \_ -> do
        need [siteDir </> "css/styles.css"]
        liftIO $ readFile $ siteDir </> "css/styles.css"

    commonData <- newCache $ \_ -> do
        cssContent <- css Anything
        imagesContent <- images Anything
        let imageGetter filePath =
                if "/images/" `isPrefixOf` filePath
                    then M.lookup (tail filePath) imagesContent
                    else Nothing

        return $ CommonData
            { _dataCss = cssContent
            , _imageMeta = imageGetter
            }

    phony "blogposts" $ do
        ps <- postsList PostsCacheById
        let postsFilePaths = map (\i -> sitePostsDir </> i </> indexHtml) $ M.keys ps
        tags <- tagsList Anything
        let tagsPaths = concatMap (\(t, fs) -> pathsFromList (siteDir </> tagDir </> t) fs) $ M.assocs tags
        need $ postsFilePaths ++ pathsFromList siteDir ps ++ tagsPaths

    sitePostsDir </> "*" </> indexHtml %> \out -> do
        cd <- commonData Anything
        ps <- postsList PostsCacheById
        let post = ps M.! idFromDestFilePath out
        -- let o = (def :: LucidWriterOptions) & Text.Pandoc.LucidWriter.commonData .~ cd
        -- putNormal $ show $ (o ^. Text.Pandoc.LucidWriter.commonData ^. imageMeta) "/images/travel/2015-09-satrip/rio-4-marius-wc-1.jpg"
        putNormal $ "Writing page " ++ out
        liftIO $ renderToFile out $ T.postPage cd post

    -- Main page
    siteDir </> indexHtml %> \out -> do
        ps <- postsList PostsCacheByDate
        cd <- commonData Anything
        let postsOnPage = getPostsForPage ps 1
        welcome <- posts "index.md"
        putNormal $ "Writing page " ++ out
        liftIO $ renderToFile out $ T.indexPage cd welcome postsOnPage

    -- Older page
    siteDir </> pageDir </> "*" </> indexHtml %> \out -> do
        ps <- postsList PostsCacheByDate
        cd <- commonData Anything
        let page = (read $ idFromDestFilePath out) :: Int
        let postsOnPage = getPostsForPage ps page
        let olderPage = getOlderPage "/" ps page
        let newerPage = getNewerPage "/" ps page
        putNormal $ "Writing page " ++ out
        liftIO $ renderToFile out $ T.listPage cd olderPage newerPage postsOnPage

    -- Tags main page
    siteDir </> tagDir </> "*" </> indexHtml %> \out -> do
        tags <- tagsList Anything
        cd <- commonData Anything
        let tag = idFromDestFilePath out
        let ps = tags M.! tag
        let postsOnPage = getPostsForPage ps 1
        let olderPage = getOlderPage ("/tag/" ++ tag ++ "/") ps 1
        putNormal $ "Writing page " ++ out
        liftIO $ renderToFile out $ T.listPage cd olderPage (Nothing) postsOnPage

    -- Tags older pages
    siteDir </> tagDir </> "*" </> pageDir </> "*" </> indexHtml %> \out -> do
        tags <- tagsList Anything
        cd <- commonData Anything
        let (tag, page) = tagAndPageFromDestFilePath out
        let ps = tags M.! tag
        let postsOnPage = getPostsForPage ps page
        let olderPage = getOlderPage ("/tag/" ++ tag ++ "/") ps page
        let newerPage = getNewerPage ("/tag/" ++ tag ++ "/") ps page
        putNormal $ "Writing page " ++ out
        liftIO $ renderToFile out $ T.listPage cd olderPage newerPage postsOnPage

    pandocCacheDir <//> "*.md" %> \out -> do
        let src = dropDirectory2 out
        putNormal $ "Reading post " ++ src
        need [src]
        file <- liftIO $ readFile src

        let pandoc = handleError $ readMarkdown readerOptions file
        let post = buildPost src pandoc
        let pCover = post ^. fileMeta ^. postCover
        color <- if isNothing $ pCover ^. coverColor
            then getImageColor image $ pCover ^. coverImg
            else return $ pCover ^. coverColor

        let updatedPost = post & fileMeta %~ (\m -> m
                & postId    .~ (fromMaybe "" $ idFromSrcFilePath src)
                & postDate  %~ alterDate src
                & postCover .~ (pCover & coverColor .~ color)
                )
        liftIO $ createDirectoryIfMissing True (takeDirectory out)
        liftIO $ B.encodeFile out updatedPost -- TODO File
    where
        buildList :: PostsCache -> [File] -> Posts
        buildList _ [] = M.empty
        buildList PostsCacheById [p] = M.singleton (p ^. fileMeta ^. postId) p
        buildList PostsCacheById (p:ps) =
            let key = p ^. fileMeta ^. postId;
                listRest = buildList PostsCacheById ps in
            if key `M.member` listRest
                then error $ "Duplicate post key " ++ key
                else M.insert key p listRest
        buildList PostsCacheByDate [p] = M.singleton (dateKey $ p ^. fileMeta ^?! postDate) p
        buildList PostsCacheByDate (p:ps) =
            let key = dateKey $ p ^. fileMeta ^?! postDate;
                listRest = buildList PostsCacheByDate ps in
            if key `M.member` listRest
                then error $ "Duplicate post key " ++ key
                else M.insert key p listRest

        buildTags :: [File] -> PostsTags
        buildTags [] = M.empty
        buildTags (f:fs) =
            M.unionWith M.union
                (M.fromList $ map (\t ->
                    (t, M.singleton (dateKey $ f ^. fileMeta ^?! postDate) f)) $ f ^. fileMeta ^. postTags)
                (buildTags fs)

        dateKey :: Maybe UTCTime -> String
        dateKey (Just time)= formatTime timeLocale (iso8601DateFormat (Just "%H:%M:%S")) time
        dateKey Nothing = error "No date defined"

        getPostsForPage ps page =
            reverse [snd $ M.elemAt i ps | i <- [listLast - rangeEnd .. listLast - rangeStart]]
            where
                listLast = M.size ps - 1
                rangeStart = (page - 1) * pageSize
                rangeEnd = min (page * pageSize - 1) listLast

        getOlderPage prefix ps page
            | page * pageSize - 1 >= listLast = Nothing
            | otherwise = Just $ prefix ++ "page/" ++ show (page + 1) ++ "/"
            where
                listLast = M.size ps - 1

        getNewerPage prefix _ page
            | page == 1 = Nothing
            | page == 2 = Just $ prefix
            | otherwise = Just $ prefix ++ "page/" ++ show (page - 1) ++ "/"

        alterDate :: FilePath -> Maybe UTCTime -> Maybe UTCTime
        alterDate filePath Nothing = dateFromFilePath filePath
        alterDate _ date = date

        getImageColor :: (FilePath -> Action ImageMeta) -> Maybe String -> Action (Maybe String)
        getImageColor image (Just filePath)
            | "/images/" `isPrefixOf` filePath = do
                meta <- image $ buildDir ++ filePath ++ ".meta"
                return $ Just $ meta ^. imageColor
            | otherwise = return Nothing
        getImageColor _ Nothing = return Nothing

        dateFromFilePath :: FilePath -> Maybe UTCTime
        dateFromFilePath = parseDate . intercalate "-" . take 3 . splitAll "-" . takeFileName

        pathsFromList :: FilePath -> Posts -> [String]
        pathsFromList prefix ps =
            (prefix </> indexHtml) : listFilePaths
            where
                (d,m) = M.size ps `divMod` pageSize
                listFilePaths = [prefix </> pageDir </> show p </> indexHtml| p <- [2 .. d + (if m == 0 then 2 else 1)]]



buildImagesCache :: Rules ()
buildImagesCache =
    imagesBuildDir <//> "*.meta" %> \out -> do
        let src' = imagesDir </> dropDirectory2 out
        let src = take (length src' - 5) src'
        need [src]
        putNormal $ "Reading image " ++ src
        meta <- getImageMeta src
        liftIO $ B.encodeFile out meta


-- Build styles
styles :: Rules ()
styles =
    siteDir </> "css/*.css" %> \out -> do
        let src = "styles" </> dropDirectory3 out -<.> "pcss"
        files <- getDirectoryFiles "." ["styles//*"]
        need (postcss : "postcss.json" : files)
        cmd (FileStdout out) postcss "-c" "postcss.json" src


-- Build images
imagesRules :: Rules ()
imagesRules = do
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
        -- putNormal $ "Copying file " ++ out
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

tagAndPageFromDestFilePath :: FilePath -> (String, Int)
tagAndPageFromDestFilePath filePath =
    case filePath =~ pat :: (String, String, String, [String]) of
        (_, _, _, [tag, page]) -> (tag, read page)
        _              -> error $ "Can't extract id from " ++ filePath
    where
        pat = tagDir ++ "/([^/]*)/" ++ pageDir ++ "/([^/]*)/index.html$"
