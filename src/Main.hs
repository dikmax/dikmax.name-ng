module Main where

import           BasicPrelude
import           Config
import           Control.Lens
import qualified Data.Binary                as B
import qualified Data.Map.Lazy              as M
import qualified Data.Text                  as T
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
    fonts
    favicons

    npmPackages


build :: Rules ()
build =
    phony "build" $ do
        need ["prerequisites"]
        need ["sync-images"]
        need ["images", "blogposts", "fonts", "favicons"]

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
        -- Can't be parallel because ImageMagick isn't thread safe
        imageCacheContent <- mapM (\i -> do
            meta <- image $ buildDir </> i ++ ".meta"
            return (T.pack i, meta)) imageFiles
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

        return $ CommonData
            { _dataCss = cssContent
            , _imageMeta = imageGetter imagesContent
            }

    phony "blogposts" $ do
        ps <- postsList PostsCacheById
        let postsFilePaths = map
                (\i -> sitePostsDir </> T.unpack i </> indexHtml) $ M.keys ps
        let ampPostsFilePaths = map
                (\i -> sitePostsDir </> T.unpack i </> ampDir </> indexHtml) $ M.keys ps
        tags <- tagsList Anything
        let tagsPaths = concatMap (\(t, fs) ->
                pathsFromList (siteDir </> tagDir </> T.unpack t) fs) $
                M.assocs tags
        need $ postsFilePaths ++ ampPostsFilePaths ++
            pathsFromList siteDir ps ++ tagsPaths

    -- Post pages
    sitePostsDir </> "*" </> indexHtml %> \out -> do
        cd <- commonData Anything
        ps <- postsList PostsCacheById
        let post = ps M.! idFromDestFilePath out
        putNormal $ "Writing page " ++ out
        liftIO $ renderToFile out $
            T.postPage False (T.defaultLayout cd (post ^. fileMeta)) cd post

    -- AMP Post pages
    sitePostsDir </> "*" </> ampDir </> indexHtml %> \out -> do
        cd <- commonData Anything
        ps <- postsList PostsCacheById
        let post = ps M.! idFromDestFilePath out
        putNormal $ "Writing page " ++ out
        liftIO $ renderToFile out $
            T.postPage True (T.ampLayout cd (post ^. fileMeta)) cd post

    -- Main page
    siteDir </> indexHtml %> \out -> do
        ps <- postsList PostsCacheByDate
        cd <- commonData Anything
        let postsOnPage = getPostsForPage ps 1
        welcome <- posts "index.md"
        putNormal $ "Writing page " ++ out
        let w = (welcome & fileMeta %~ postUrl .~ domain ++ "/")
        liftIO $ renderToFile out $
            T.indexPage
                (T.defaultLayout cd (w ^. fileMeta))
                w postsOnPage

    -- Older page
    siteDir </> pageDir </> "*" </> indexHtml %> \out -> do
        ps <- postsList PostsCacheByDate
        cd <- commonData Anything
        let page = (read $ idFromDestFilePath out) :: Int
        let postsOnPage = getPostsForPage ps page
        let olderPage = getOlderPage "/" ps page
        let newerPage = getNewerPage "/" ps page
        let pageMeta = def {_postTitle = show page ++ "-я страница"}
        putNormal $ "Writing page " ++ out
        liftIO $ renderToFile out $
            T.listPage
                (T.defaultLayout cd
                    (pageMeta & postUrl .~ domain ++ "/page/" ++ show page ++ "/"))
                olderPage newerPage postsOnPage

    -- Tags main page
    siteDir </> tagDir </> "*" </> indexHtml %> \out -> do
        tags <- tagsList Anything
        cd <- commonData Anything
        let tag = idFromDestFilePath out
        let ps = tags M.! tag
        let postsOnPage = getPostsForPage ps 1
        let olderPage = getOlderPage ("/tag/" ++ tag ++ "/") ps 1
        let pageMeta = def {_postTitle = "\"" ++ tag ++ "\""}
        putNormal $ "Writing page " ++ out
        liftIO $ renderToFile out $
            T.listPage
                (T.defaultLayout cd
                    (pageMeta & postUrl .~ domain ++ "/tag/" ++ tag ++ "/"))
                olderPage Nothing postsOnPage

    -- Tags older pages
    siteDir </> tagDir </> "*" </> pageDir </> "*" </> indexHtml %> \out -> do
        tags <- tagsList Anything
        cd <- commonData Anything
        let (tag, page) = tagAndPageFromDestFilePath out
        let ps = tags M.! tag
        let postsOnPage = getPostsForPage ps page
        let olderPage = getOlderPage ("/tag/" ++ tag ++ "/") ps page
        let newerPage = getNewerPage ("/tag/" ++ tag ++ "/") ps page
        let pageMeta = def {_postTitle = "\"" ++ tag ++ "\", " ++ show page ++ "-я страница"}
        putNormal $ "Writing page " ++ out
        liftIO $ renderToFile out $
            T.listPage
                (T.defaultLayout cd
                    (pageMeta & postUrl .~ domain ++ "/tag/" ++ tag ++ "/page" ++ show page ++ "/"))
                olderPage newerPage postsOnPage

    pandocCacheDir <//> "*.md" %> \out -> do
        let src = dropDirectory2 out
        putNormal $ "Reading post " ++ src
        need [src]
        file <- liftIO $ readFile src

        let pandoc = handleError $ readMarkdown readerOptions (T.unpack file)
        let post = buildPost src pandoc
        let pCover = post ^. fileMeta ^. postCover
        imagesContent <- images Anything
        let color = mplus
                (pCover ^. coverColor)
                (maybe
                    Nothing
                    (\img -> (^. imageColor) <$> imageGetter imagesContent img)
                    (pCover ^. coverImg))

        let updatedPost = post & fileMeta %~ (\m -> m
                & postId    .~ (fromMaybe "" $ idFromSrcFilePath src)
                & postDate  %~ alterDate src
                & postCover .~ (pCover & coverColor .~ color)
                & postUrl   .~ (maybe "" (\i -> domain ++ "/post/" ++ i
                                    ++ "/") $ idFromSrcFilePath src)
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
                then terror $ "Duplicate post key " ++ key
                else M.insert key p listRest
        buildList PostsCacheByDate [p] = M.singleton (dateKey $ p ^. fileMeta ^?! postDate) p
        buildList PostsCacheByDate (p:ps) =
            let key = dateKey $ p ^. fileMeta ^?! postDate;
                listRest = buildList PostsCacheByDate ps in
            if key `M.member` listRest
                then terror $ "Duplicate post key " ++ key
                else M.insert key p listRest

        buildTags :: [File] -> PostsTags
        buildTags [] = M.empty
        buildTags (f:fs) =
            M.unionWith M.union
                (M.fromList $ map (\t ->
                    (t, M.singleton (dateKey $ f ^. fileMeta ^?! postDate) f)) $ f ^. fileMeta ^. postTags)
                (buildTags fs)

        dateKey :: Maybe UTCTime -> Text
        dateKey (Just time)= T.pack $ formatTime timeLocale (iso8601DateFormat (Just "%H:%M:%S")) time
        dateKey Nothing = terror "No date defined"

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

        dateFromFilePath :: FilePath -> Maybe UTCTime
        dateFromFilePath = parseDate . intercalate "-" . take 3 . splitAll "-" . takeFileName

        pathsFromList :: FilePath -> Posts -> [FilePath]
        pathsFromList prefix ps =
            (prefix </> indexHtml) : listFilePaths
            where
                (d,m) = M.size ps `divMod` pageSize
                listFilePaths =
                    [ prefix </> pageDir </> T.unpack (show p) </> indexHtml |
                        p <- [2 .. d + (if m == 0 then 2 else 1)] ]

        imageGetter :: Images -> Text -> Maybe ImageMeta
        imageGetter imagesContent filePath =
            if "/images/" `T.isPrefixOf` filePath
                then M.lookup (T.tail filePath) imagesContent
                else Nothing



-- Build styles
styles :: Rules ()
styles =
    siteDir </> "css/*.css" %> \out -> do
        let src = "styles" </> dropDirectory3 out -<.> "pcss"
        files <- getDirectoryFiles "." ["styles//*"]
        need (postcss : "postcss.json" : files)
        cmd (FileStdout out) postcss ("-c" :: FilePath) ("postcss.json" :: FilePath) src


-- Build images
imagesRules :: Rules ()
imagesRules = do
    imagesBuildDir <//> "*.meta" %> \out -> do
        let src' = imagesDir </> dropDirectory2 out
        let src = take (length src' - 5) src'
        need [src]
        putNormal $ "Reading image " ++ src
        meta <- getImageMeta src
        liftIO $ B.encodeFile out meta

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

fonts :: Rules ()
fonts = do
    phony "fonts" $ do
        fontFiles <- getDirectoryFiles "." ["fonts/*"]
        need [siteDir </> x | x <- fontFiles]

    buildStatic "fonts/*"

favicons :: Rules ()
favicons = do
    phony "favicons" $ do
        faviconsFiles <- getDirectoryFiles "." ["favicons/*"]
        forM_ faviconsFiles (\src -> do
            let out = siteDir </> dropDirectory1 src
            copyFileChanged src out)

-- npm packages
npmPackages :: Rules ()
npmPackages =
    [postcss] &%> \_ -> do
        need ["package.json"]
        cmd ("npm" :: FilePath) ("install" :: FilePath)

-- Static files, that just should be copied to `siteDir`
buildStatic :: FilePath -> Rules ()
buildStatic filePath =
    siteDir </> filePath %> \out -> do
        let src = dropDirectory2 out
        -- putNormal $ "Copying file " ++ out
        copyFileChanged src out


idFromPost :: Pandoc -> Text
idFromPost (Pandoc meta _) = maybe (terror "Post have no id") getId $ lookupMeta "id" meta
    where
        getId (MetaString s) = T.pack s
        getId s = terror $ "Post id field have wrong value: " ++ show s

dateFromPost :: Pandoc -> Text
dateFromPost (Pandoc meta _) = maybe (terror "Post have no date") getDate $ lookupMeta "date" meta
    where
        getDate (MetaString s) = T.pack s
        getDate s = terror $ "Post date field have wrong value: " ++ show s

idFromSrcFilePath :: FilePath -> Maybe Text
idFromSrcFilePath filePath =
    case filePath =~ pat :: (String, String, String, [String]) of
        (_, _, _, [v]) -> Just $ T.pack v
        _              -> Nothing
    where
        pat :: String
        pat = "/[0-9]{4}/[0-9]{4}-[0-9]{2}-[0-9]{2}-(.*)\\.md$"

idFromDestFilePath :: FilePath -> Text
idFromDestFilePath filePath =
    case filePath =~ pat :: (String, String, String, [String]) of
        (_, _, _, v : _) -> T.pack v
        _              -> error $ "Can't extract id from " ++ filePath
    where
        pat :: String
        pat = "/([^/]*)(/amp)?/index\\.html$"

tagAndPageFromDestFilePath :: FilePath -> (Text, Int)
tagAndPageFromDestFilePath filePath =
    case filePath =~ pat :: (String, String, String, [String]) of
        (_, _, _, [tag, page]) -> (T.pack tag, read $ T.pack page)
        _              -> error $ "Can't extract id from " ++ filePath
    where
        pat = tagDir ++ "/([^/]*)/" ++ pageDir ++ "/([^/]*)/index.html$"
