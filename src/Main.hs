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
import           JsonLD
import           Lib
import           Lucid                      hiding (command_)
import           Map
import           Rules
import           System.Directory           (createDirectoryIfMissing)
import           Server
import qualified Template                   as T
import           Text.Pandoc
import           Text.Pandoc.Error          (handleError)
import           Types

readerOptions :: ReaderOptions
readerOptions = def
    { readerSmart = True
    , readerParseRaw = True
    }

main :: IO ()
main = do
    args <- getArgs
    if (length args > 0 && head args == "server")
    then runServer
    else shakeArgs options $ do
        usingConfigFile "build.cfg"
        want ["build"]
        clean
        deploy

        prerequisites
        build
        mapData
        styles
        scripts
        imagesRules
        blog
        favicons
        demos
        robotsTxt

        npmPackages
        compress


build :: Rules ()
build =
    phony "build" $ do
        need ["prerequisites"]
        need ["sync-images"]
        need ["phony-images", "blogposts", "phony-favicons", "phony-demos"
            , siteDir </> "robots.txt"
            , siteDir </> T.unpack rssFeedFile
            , siteDir </> "scripts/main.js"
            , siteDir </> "scripts/map.js"
            , siteDir </> "data/world.json"
            ]

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
            , siteDir </> "map/list/" </> indexHtml
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
        let w = welcome & fileMeta %~ (postUrl .~ domain ++ "/")
        let meta = w ^. fileMeta & postMeta .~ (toMetadata $ WebPage
                 { _webPageHeadline = "[dikmax's blog]"
                 , _webPageCopyrightHolder = copyrightHolder
                 , _webPageCopyrightYear = copyrightYear
                 })

        liftIO $ renderToFile out $
            T.indexPage
                (T.defaultLayout cd meta)
                cd w postsOnPage


    -- Older page
    siteDir </> pageDir </> "*" </> indexHtml %> \out -> do
        ps <- postsList PostsCacheByDate
        cd <- commonData Anything
        let page = (read $ idFromDestFilePath out) :: Int
        let postsOnPage = getPostsForPage ps page
        let olderPage = getOlderPage "/" ps page
        let newerPage = getNewerPage "/" ps page
        let title = show page ++ "-я страница"
        let pageMeta = def
                { _postTitle = title
                , _postUrl = domain ++ "/page/" ++ show page ++ "/"
                , _postMeta  = toMetadata $ WebPage
                      { _webPageHeadline = title
                      , _webPageCopyrightHolder = copyrightHolder
                      , _webPageCopyrightYear = copyrightYear
                      }
                }
        putNormal $ "Writing page " ++ out
        liftIO $ renderToFile out $
            T.listPage
                (T.defaultLayout cd pageMeta)
                cd olderPage newerPage postsOnPage


    -- Tags main page
    siteDir </> tagDir </> "*" </> indexHtml %> \out -> do
        tags <- tagsList Anything
        cd <- commonData Anything
        let tag = idFromDestFilePath out
        when (tag `M.notMember` tags) $ putNormal $ "Tag " ++ T.unpack tag ++ "not found"
        let ps = tags M.! tag
        let postsOnPage = getPostsForPage ps 1
        let olderPage = getOlderPage ("/tag/" ++ tag ++ "/") ps 1
        let title = "\"" ++ tag ++ "\""
        let pageMeta = def
                { _postTitle = title
                , _postUrl = domain ++ "/tag/" ++ tag ++ "/"
                , _postMeta  = toMetadata $ WebPage
                      { _webPageHeadline = title
                      , _webPageCopyrightHolder = copyrightHolder
                      , _webPageCopyrightYear = copyrightYear
                      }
                }
        putNormal $ "Writing page " ++ out
        liftIO $ renderToFile out $
            T.listPage
                (T.defaultLayout cd pageMeta)
                cd olderPage Nothing postsOnPage


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
        let title = "\"" ++ tag ++ "\", " ++ show page ++ "-я страница"
        let pageMeta = def
                { _postTitle = title
                , _postUrl = domain ++ "/tag/" ++ tag ++ "/page" ++ show page ++ "/"
                , _postMeta  = toMetadata $ WebPage
                      { _webPageHeadline = title
                      , _webPageCopyrightHolder = copyrightHolder
                      , _webPageCopyrightYear = copyrightYear
                      }
                }
        putNormal $ "Writing page " ++ out
        liftIO $ renderToFile out $
            T.listPage
                (T.defaultLayout cd pageMeta)
                cd olderPage newerPage postsOnPage


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
        let meta = def
                { _postTitle = "Архив"
                , _postUrl = domain ++ "/archive/"
                , _postMeta  = toMetadata $ WebPage
                      { _webPageHeadline = "Архив"
                      , _webPageCopyrightHolder = copyrightHolder
                      , _webPageCopyrightYear = copyrightYear
                      }
                }
        liftIO $ renderToFile out $
            T.archivePage
                (T.defaultLayout cd meta) files


    -- About page
    siteDir </> "about" </> indexHtml %> \out -> do
        cd <- commonData Anything
        about <- posts "about.md"
        putNormal $ "Writing page " ++ out
        let a = about & fileMeta %~ postUrl .~ domain ++ "/"
                      & fileMeta %~ postMeta .~
                        (toMetadata $ AboutPage
                        { _aboutPageHeadline = "Обо мне"
                        , _aboutPageCopyrightHolder = copyrightHolder
                        , _aboutPageCopyrightYear = copyrightYear
                        })
        liftIO $ renderToFile out $
            T.aboutPage
                (T.defaultLayout cd (a ^. fileMeta)) cd a


    -- Map pages
    siteDir </> "map" </> indexHtml %> \out -> do
        cd <- commonData Anything
        need ["data/map.json"]

        mapJson <- liftIO $ LBS.readFile "data/map.json"
        let meta = def
                { _postTitle = "Путешествия"
                , _postUrl = domain ++ "/map/"
                , _postMeta  = toMetadata $ WebPage
                      { _webPageHeadline = "Путешествия"
                      , _webPageCopyrightHolder = copyrightHolder
                      , _webPageCopyrightYear = copyrightYear
                      }
                }

        putNormal $ "Writing page " ++ out
        liftIO $ renderToFile out $
            T.mapPage (T.mapLayout cd meta)

    siteDir </> "map/list" </> indexHtml %> \out -> do
        cd <- commonData Anything
        need ["data/map.json"]

        mapJson <- liftIO $ LBS.readFile "data/map.json"
        let res = A.eitherDecode mapJson :: Either String MapCountries
        let meta = def
                { _postTitle = "Путешествия"
                , _postUrl = domain ++ "/map/list/"
                , _postMeta  = toMetadata $ WebPage
                      { _webPageHeadline = "Путешествия"
                      , _webPageCopyrightHolder = copyrightHolder
                      , _webPageCopyrightYear = copyrightYear
                      }
                }

        either error (\countries -> do
            putNormal $ "Writing page " ++ out
            liftIO $ renderToFile out $
                T.mapListPage
                    (T.defaultLayout cd meta) countries
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

        imagesContent <- images Anything

        let pandoc = handleError $ readMarkdown readerOptions (T.unpack file)
        let post = buildPost src imagesContent pandoc
        let pCover = post ^. fileMeta ^. postCover
        let color = mplus
                (pCover ^. coverColor)
                (maybe
                    Nothing
                    (\img -> (^. imageColor) <$> imageGetter imagesContent img)
                    (pCover ^. coverImg))

        -- TODO move to build post
        let updatedPost = post & fileMeta %~ (\m -> m
                & postDate  %~ alterDate src
                & postCover .~ (pCover & coverColor .~ color)
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

-- Build map data

{-
rm map/subunits.json
rm map/countries.json
rm map/regions.json
ogr2ogr -f GeoJSON map/subunits.json -where "ADM0_A3 = 'FRA'" map/ne_10m_admin_0_map_subunits/ne_10m_admin_0_map_subunits.shp
ogr2ogr -f GeoJSON map/countries.json -where "ADM0_A3 != 'FRA' and ADM0_A3 != 'RUS' and ADM0_A3 != 'USA'" map/ne_10m_admin_0_countries_lakes/ne_10m_admin_0_countries_lakes.shp
ogr2ogr -f GeoJSON map/regions.json -where "ADM0_A3 = 'RUS' or ADM0_A3 = 'USA'" map/ne_10m_admin_1_states_provinces_lakes/ne_10m_admin_1_states_provinces_lakes.shp
topojson -o map/world.json --id-property ADM_A3,SU_A3,adm1_code --simplify 1e-6 -- map/countries.json map/subunits.json map/regions.json
-}

mapData :: Rules ()
mapData =
    siteDir </> "data/world.json" %> \out -> do
        let subunitsSrc = "map/ne_10m_admin_0_map_subunits/ne_10m_admin_0_map_subunits.shp"
        let countriesSrc = "map/ne_10m_admin_0_countries_lakes/ne_10m_admin_0_countries_lakes.shp"
        let regionsSrc = "map/ne_10m_admin_1_states_provinces_lakes/ne_10m_admin_1_states_provinces_lakes.shp"
        let subunits = buildDir </> "map/subunits.json"
        let countries = buildDir </> "map/countries.json"
        let regions = buildDir </> "map/regions.json"

        need [subunitsSrc, countriesSrc, regionsSrc, topojson]

        liftIO $ createDirectoryIfMissing True (buildDir </> "map")

        liftIO $ removeFiles "." [subunits, countries, regions]
        command_ [] "ogr2ogr" ["-f", "GeoJSON", subunits,
            "-where", "ADM0_A3 = 'FRA'", subunitsSrc]
        command_ [] "ogr2ogr" ["-f", "GeoJSON", countries,
            "-where", "ADM0_A3 != 'FRA' and ADM0_A3 != 'RUS' and ADM0_A3 != 'USA'", countriesSrc]
        command_ [] "ogr2ogr" ["-f", "GeoJSON", regions,
            "-where", "ADM0_A3 = 'RUS' or ADM0_A3 = 'USA'", regionsSrc]

        liftIO $ createDirectoryIfMissing True (takeDirectory out)

        command_ [] topojson ["-o", out,
            "--id-property", "ADM_A3,SU_A3,adm1_code",
            "--simplify", "1e-6", "--", countries, subunits, regions]



-- Build styles
styles :: Rules ()
styles =
    siteDir </> "css/*.css" %> \out -> do
        let src = "styles" </> dropDirectory3 out -<.> "pcss"
        files <- getDirectoryFiles "." ["styles//*"]
        need (postcss : "postcss.json" : files)
        cmd (FileStdout out) postcss ("-c" :: FilePath) ("postcss.json" :: FilePath) src


favicons :: Rules ()
favicons =
    phony "phony-favicons" $ do
        faviconsFiles <- getDirectoryFiles "." ["favicons/*"]
        forM_ faviconsFiles (\src -> do
            let out = siteDir </> dropDirectory1 src
            copyFileChanged src out)

-- npm packages
npmPackages :: Rules ()
npmPackages =
    [postcss, topojson, "node_modules/d3/d3.min.js"] &%> \_ -> do
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

coverToStyle :: File -> Text
coverToStyle file =
    maybe "" (\i -> "background-image:url(" ++ i ++ ");") (cover ^. coverImg)
        ++ maybe "" (\i -> "background-color:" ++ i ++ ";") (cover ^. coverColor)
        ++ "background-position-x:" ++ (cover ^. coverHCenter) ++ ";"
        ++ "background-position-y:" ++ (cover ^. coverVCenter)
    where
        cover = file ^. fileMeta ^. postCover
