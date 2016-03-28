module Main where

import           BasicPrelude
import           Collections
import           Compress
import           Config
import           Control.Lens
import qualified Data.Aeson                 as A
import qualified Data.Binary                as B
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Map.Lazy              as M
import qualified Data.Text                  as T
import           Data.Time
import           Data.Yaml
import           Development.Shake
import           Development.Shake.Config
import           Development.Shake.FilePath
import           Images
import           Lib
import           Lucid                      hiding (command_)
import           Map
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
    deploy

    runServer
    prerequisites
    build
    styles
    scripts
    imagesRules
    blog
    favicons
    demos

    npmPackages
    compress


build :: Rules ()
build =
    phony "build" $ do
        need ["prerequisites"]
        need ["sync-images"]
        need ["images", "blogposts", "favicons", "demos",
            siteDir </> T.unpack rssFeedFile, siteDir </> "scripts/main.js"]

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


    -- All images metadata
    images <- newCache $ \_ -> do
        imageFiles <- getDirectoryFiles "." imagesPatterns
        -- Can't be parallel because ImageMagick isn't thread safe
        imageCacheContent <- mapM (\i -> do
            meta <- image $ buildDir </> i ++ ".meta"
            return (T.pack i, meta)) imageFiles
        return $ M.fromList imageCacheContent


    -- All posts metadata
    postsList <- newCache $ \t -> do
        postCacheContent <- preparePostsCache posts
        return $ buildList t postCacheContent


    -- All posts grouped be tags
    tagsList <- newCache $ \_ -> do
        postCacheContent <- preparePostsCache posts
        return $ buildTags postCacheContent


    collectionsList <- newCache $ \_ -> do
        need ["data/collections.yaml"]
        -- postCacheContent <- preparePostsCache posts
        res <- liftIO $ decodeFileEither "data/collections.yaml"
        either (error . prettyPrintParseException) return res :: Action Collections


    css <- newCache $ \_ -> do
        need [siteDir </> "css/styles.css"]
        liftIO $ readFile $ siteDir </> "css/styles.css"


    commonData <- newCache $ \_ -> do
        cssContent <- css Anything
        imagesContent <- images Anything
        cl <- collectionsList Anything

        return CommonData
            { _dataCss = cssContent
            , _imageMeta = imageGetter imagesContent
            , _collections = cl
            }


    phony "blogposts" $ do
        ps <- postsList PostsCacheById
        let postsFilePaths = map
                (\i -> sitePostsDir </> T.unpack i </> indexHtml) $ M.keys ps
        let ampPostsFilePaths = map
                (\i -> sitePostsDir </> T.unpack i </> ampDir </> indexHtml) $ M.keys ps
        tags <- tagsList Anything
        let tagsPaths = concatMap (\(t, fs) ->
                pathsFromList (siteDir </> tagDir </> T.unpack t) indexHtml fs) $
                M.assocs tags

        need $ postsFilePaths ++ ampPostsFilePaths ++
            pathsFromList siteDir indexHtml ps ++ tagsPaths ++
            [ siteDir </> "about" </> indexHtml
            , siteDir </> "archive" </> indexHtml
            , siteDir </> "map" </> indexHtml
            , siteDir </> "404" </> indexHtml
            , siteDir </> "sitemap.xml"
            ]


    -- Post pages
    sitePostsDir </> "*" </> indexHtml %> \out -> do
        cd <- commonData Anything
        ps <- postsList PostsCacheById
        psd <- postsList PostsCacheByDate
        let post = ps M.! idFromDestFilePath out
        let postIndex = M.findIndex (dateKey $ post ^. fileMeta ^?! postDate) psd
        let prevPage =
                if (postIndex == 0)
                then Nothing
                else Just $ snd $ M.elemAt (postIndex - 1) psd
        let nextPage =
                if (postIndex == M.size psd - 1)
                then Nothing
                else Just $ snd $ M.elemAt (postIndex + 1) psd
        let postCd = cd & dataCss %~
                (++ ".header_for-post:before{" ++ coverToStyle post ++ "}")

        putNormal $ "Writing page " ++ out
        liftIO $ renderToFile out $
            T.postPage False (T.defaultLayout postCd (post ^. fileMeta)) cd post
                prevPage nextPage


    -- AMP Post pages
    sitePostsDir </> "*" </> ampDir </> indexHtml %> \out -> do
        cd <- commonData Anything
        ps <- postsList PostsCacheById
        psd <- postsList PostsCacheByDate
        let post = ps M.! idFromDestFilePath out
        let postIndex = M.findIndex (dateKey $ post ^. fileMeta ^?! postDate) psd
        let prevPage =
                if (postIndex == 0)
                then Nothing
                else Just $ snd $ M.elemAt (postIndex - 1) psd
        let nextPage =
                if (postIndex == M.size psd - 1)
                then Nothing
                else Just $ snd $ M.elemAt (postIndex + 1) psd

        let postCd = cd & dataCss %~
                (++ ".header_for-post:before{" ++ coverToStyle post ++ "}")
        putNormal $ "Writing page " ++ out
        liftIO $ renderToFile out $
            T.postPage True (T.ampLayout postCd (post ^. fileMeta)) cd post
                prevPage nextPage


    -- Main page
    siteDir </> indexHtml %> \out -> do
        ps <- postsList PostsCacheByDate
        cd <- commonData Anything
        let postsOnPage = getPostsForPage ps 1
        welcome <- posts "index.md"
        putNormal $ "Writing page " ++ out
        let w = welcome & fileMeta %~ postUrl .~ domain ++ "/"
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
        when (tag `M.notMember` tags) $ putNormal $ "Tag " ++ T.unpack tag ++ "not found"
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
        when (tag `M.notMember` tags) $ putNormal $ "Tag " ++ T.unpack tag ++ "not found"
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


    -- RSS feed
    siteDir </> T.unpack rssFeedFile %> \out -> do
        ps <- postsList PostsCacheByDate
        -- cd <- commonData Anything
        let postsOnPage = getPostsForPage ps 1
        -- welcome <- posts "index.md"
        putNormal $ "Writing feed " ++ out
        liftIO $ do
            now <- getCurrentTime
            renderToFile out $ T.feedPage now postsOnPage


    -- Archive page
    siteDir </> "archive" </> indexHtml %> \out -> do
        ps <- postsList PostsCacheByDate
        cd <- commonData Anything
        let files = M.elems ps
        putNormal $ "Writing page " ++ out
        liftIO $ renderToFile out $
            T.archivePage
                (T.defaultLayout cd def) files


    -- About page
    siteDir </> "about" </> indexHtml %> \out -> do
        cd <- commonData Anything
        about <- posts "about.md"
        putNormal $ "Writing page " ++ out
        let a = about & fileMeta %~ postUrl .~ domain ++ "/"
        liftIO $ renderToFile out $
            T.aboutPage
                (T.defaultLayout cd (a ^. fileMeta)) a


    -- Map page
    siteDir </> "map" </> indexHtml %> \out -> do
        cd <- commonData Anything
        need ["data/map.json"]

        mapJson <- liftIO $ LBS.readFile "data/map.json"
        let res = A.eitherDecode mapJson :: Either String MapCountries
        either error (\countries -> do
            putNormal $ "Writing page " ++ out
            liftIO $ renderToFile out $
                T.mapPage
                    (T.defaultLayout cd def) countries
            ) res


    -- 404 error
    siteDir </> "404" </> indexHtml %> \out -> do
        cd <- commonData Anything
        notFound <- posts "404.md"
        putNormal $ "Writing page " ++ out
        let nf = notFound & fileMeta %~ postUrl .~ domain ++ "/"
        liftIO $ renderToFile out $
            T.notFoundPage
                (T.defaultLayout cd (nf ^. fileMeta))
                nf

    -- Sitemap
    siteDir </> "sitemap.xml" %> \out -> do
        ps <- postsList PostsCacheById
        let postsFilePaths = map
                (\i -> def & suLoc .~ ("/post/" ++ i ++ "/")) $ M.keys ps

        putNormal $ "Writing sitemap " ++ out
        liftIO $ renderToFile out $ T.sitemapPage $
            [ def { _suLoc = "/", _suPriority = "0.8" }
            , def { _suLoc = "/about/", _suPriority = "0.5" }
            ] ++ postsFilePaths


    -- Parse Markdown with metadata and save to temp file
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
                & postId    .~ fromMaybe "" (idFromSrcFilePath src)
                & postDate  %~ alterDate src
                & postCover .~ (pCover & coverColor .~ color)
                & postUrl   .~ maybe "" (\i -> domain ++ "/post/" ++ i
                                    ++ "/") (idFromSrcFilePath src)
                )
        liftIO $ createDirectoryIfMissing True (takeDirectory out)
        liftIO $ B.encodeFile out updatedPost -- TODO File
    where
        preparePostsCache :: (FilePath -> Action File) -> Action [File]
        preparePostsCache posts = do
            postFiles <- getDirectoryFiles "." ["posts//*.md"]
            -- Preparing parsed data in parallel
            need $ map (pandocCacheDir </>) postFiles
            mapM posts postFiles

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
        buildTags = foldr
            (\f -> M.unionWith M.union
                (M.fromList $ map
                    (\t -> (t, M.singleton (dateKey $ f ^. fileMeta ^?! postDate) f)) $
                    f ^. fileMeta ^. postTags))
            M.empty

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
            | page == 2 = Just prefix
            | otherwise = Just $ prefix ++ "page/" ++ show (page - 1) ++ "/"

        alterDate :: FilePath -> Maybe UTCTime -> Maybe UTCTime
        alterDate filePath Nothing = dateFromFilePath filePath
        alterDate _ date = date

        dateFromFilePath :: FilePath -> Maybe UTCTime
        dateFromFilePath = parseDate . intercalate "-" . take 3 . splitAll "-" . takeFileName

        pathsFromList :: FilePath -> FilePath -> Posts -> [FilePath]
        pathsFromList prefix suffix ps =
            (prefix </> suffix) : listFilePaths
            where
                (d,m) = M.size ps `divMod` pageSize
                listFilePaths =
                    [ prefix </> pageDir </> T.unpack (show p) </> suffix |
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

-- Build scripts
scripts :: Rules ()
scripts = do
    highlightJsPack %> \_ -> do
        need ["scripts/highlight.js/package.json"]
        command_ [Cwd "scripts/highlight.js/"] "npm" ["install"]
        command_ [Cwd "scripts/highlight.js/"] "node" ("tools/build.js" : "-t" :
            "browser" : includeHighlightingLanguages)

    siteDir </> "scripts/main.js" %> \out -> do
        let src = dropDirectory2 out
        files <- getDirectoryFiles "." ["scripts//*"]
        need (postcss : highlightJsPack : files)
        h <- liftIO $ BS.readFile highlightJsPack
        Stdout my <- command [] "java"
            [ "-client", "-jar", "node_modules/google-closure-compiler/compiler.jar"
            , "--entry_point", "goog:dikmax.main"
            , "--only_closure_dependencies", "true"
            , "--compilation_level", "ADVANCED_OPTIMIZATIONS"
            , "--warning_level", "VERBOSE"
            , "--language_in", "ECMASCRIPT6_STRICT"
            , "--language_out", "ECMASCRIPT5_STRICT"
            , "--externs", "scripts/externs/highlight.js"
            , "--js", "node_modules/google-closure-library/closure/goog/**.js"
            , "--js", "!node_modules/google-closure-library/closure/goog/**_test.js"
            , "--js", "scripts/dikmax/*.js"
            , "--js", src]
        liftIO $ BS.writeFile out (h ++ my)

    siteDir </> "scripts/map.js" %> \out -> do
        let src = dropDirectory2 out
        -- let d3File = "node_modules/d3/d3.min.js"
        -- let topojsonFile = "node_modules/topojson/build/topojson.min.js"
        files <- getDirectoryFiles "." ["scripts//*"]
        need (postcss : files)
        -- d3 <- liftIO $ BS.readFile d3File
        -- topojson <- liftIO $ BS.readFile topojsonFile
        Stdout my <- command [] "java"
            [ "-client", "-jar", "node_modules/google-closure-compiler/compiler.jar"
            , "--entry_point", "goog:dikmax.main"
            , "--only_closure_dependencies", "true"
            , "--compilation_level", "ADVANCED_OPTIMIZATIONS"
            , "--warning_level", "VERBOSE"
            , "--language_in", "ECMASCRIPT6_STRICT"
            , "--language_out", "ECMASCRIPT5_STRICT"
            -- , "--externs", "scripts/externs/d3.js"
            -- , "--externs", "scripts/externs/topojson.js"
            , "--js", "node_modules/google-closure-library/closure/goog/**.js"
            , "--js", "!node_modules/google-closure-library/closure/goog/**_test.js"
            , "--js", "scripts/dikmax/*.js"
            , "--js", src]
        liftIO $ BS.writeFile out my

    where
        highlightJsPack :: FilePath
        highlightJsPack = "scripts/highlight.js/build/highlight.pack.js"

favicons :: Rules ()
favicons =
    phony "favicons" $ do
        faviconsFiles <- getDirectoryFiles "." ["favicons/*"]
        forM_ faviconsFiles (\src -> do
            let out = siteDir </> dropDirectory1 src
            copyFileChanged src out)

-- npm packages
npmPackages :: Rules ()
npmPackages =
    [postcss, "node_modules/d3/d3.min.js"] &%> \_ -> do
        need ["package.json"]
        cmd ("npm" :: FilePath) ("install" :: FilePath)


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

coverToStyle :: File -> Text
coverToStyle file =
    maybe "" (\i -> "background-image:url(" ++ i ++ ");") (cover ^. coverImg)
        ++ maybe "" (\i -> "background-color:" ++ i ++ ";") (cover ^. coverColor)
        ++ "background-position-x:" ++ (cover ^. coverHCenter) ++ ";"
        ++ "background-position-y:" ++ (cover ^. coverVCenter)
    where
        cover = file ^. fileMeta ^. postCover
