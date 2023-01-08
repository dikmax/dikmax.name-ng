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
import           Data.Time.Format.ISO8601
import           Data.Yaml
import           Development.Shake
import           Development.Shake.Config
import           Development.Shake.FilePath
import           Images
import           JsonLD
import           Lib
import           Lucid                      hiding (command_)
import           Map
import           Media
import           Rules
import           System.Directory           (createDirectoryIfMissing)
import           Server
import qualified Template                   as T
import qualified Template.Common            as T
import           Text.Pandoc                hiding (getCurrentTime)
import           Text.Pandoc.Error          (handleError)
import           Types

readerOptions :: ReaderOptions
readerOptions = def
    { readerExtensions = pandocExtensions
    }

main :: IO ()
main = do
    args <- getArgs
    if not (null args) && head args == "server"
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
        mediaRules
        blog
        fonts
        favicons
        demos
        robotsTxt

        npmPackages
        compress


build :: Rules ()
build =
    phony "build" $ do
        need ["prerequisites"]
        need ["phony-images", "phony-media", "blogposts", "phony-fonts"
            , "phony-favicons", "phony-demos"
            , siteDir </> "robots.txt"
            , siteDir </> T.unpack rssFeedFile
            , siteDir </> "css/cookieconsent.css"
            , siteDir </> "scripts/main.js"
            , siteDir </> "scripts/map.js"
            , siteDir </> "data/world.json"
            ]
        need ["compress-images"]

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
            , siteDir </> "privacy" </> indexHtml
            , siteDir </> "map" </> indexHtml
            , siteDir </> "map/list/" </> indexHtml
            , siteDir </> "404" </> indexHtml
            , siteDir </> "404.html"
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
                if postIndex == 0
                then Nothing
                else Just $ snd $ M.elemAt (postIndex - 1) psd
        let nextPage =
                if postIndex == M.size psd - 1
                then Nothing
                else Just $ snd $ M.elemAt (postIndex + 1) psd
        let postCd = cd & dataCss %~
                (++ ".header_for-post:before{" ++ T.coverToStyle post ++ "}")

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
                if postIndex == 0
                then Nothing
                else Just $ snd $ M.elemAt (postIndex - 1) psd
        let nextPage =
                if postIndex == M.size psd - 1
                then Nothing
                else Just $ snd $ M.elemAt (postIndex + 1) psd

        let postCd = cd & dataCss %~
                (++ ".header_for-post:before{" ++ T.coverToStyle post ++ "}")
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
        let meta = w ^. fileMeta & postMeta .~ toMetadata WebPage
                 { _webPageHeadline = "[dikmax's blog]"
                 , _webPageCopyrightHolder = copyrightHolder
                 , _webPageCopyrightYear = copyrightYear
                 }

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
        let title = tshow page ++ "-я страница"
        let pageMeta = def
                { _postTitle = title
                , _postUrl = domain ++ "/page/" ++ tshow page ++ "/"
                , _postMeta  = toMetadata WebPage
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
                , _postMeta  = toMetadata WebPage
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
        let title = "\"" ++ tag ++ "\", " ++ tshow page ++ "-я страница"
        let pageMeta = def
                { _postTitle = title
                , _postUrl = domain ++ "/tag/" ++ tag ++ "/page" ++ tshow page ++ "/"
                , _postMeta  = toMetadata WebPage
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

        need [siteDir </> "css/archive.css"]
        archiveCss <- liftIO $ readFile $ siteDir </> "css/archive.css"

        let files = M.elems ps
        putNormal $ "Writing page " ++ out
        let meta = def
                { _postTitle = "Архив"
                , _postUrl = domain ++ "/archive/"
                , _postMeta  = toMetadata WebPage
                      { _webPageHeadline = "Архив"
                      , _webPageCopyrightHolder = copyrightHolder
                      , _webPageCopyrightYear = copyrightYear
                      }
                }
        liftIO $ renderToFile out $
            T.archivePage
                (T.defaultLayout (cd & dataCss .~ archiveCss) meta) files


    -- About page
    siteDir </> "about" </> indexHtml %> \out -> do
        cd <- commonData Anything
        about <- posts "about.md"

        need [siteDir </> "css/about.css"]
        aboutCss <- liftIO $ readFile $ siteDir </> "css/about.css"

        putNormal $ "Writing page " ++ out
        let a = about & fileMeta %~ postUrl .~ domain ++ "/"
                      & fileMeta %~ postMeta .~
                        toMetadata AboutPage
                            { _aboutPageHeadline = "Обо мне"
                            , _aboutPageCopyrightHolder = copyrightHolder
                            , _aboutPageCopyrightYear = copyrightYear
                            }
        liftIO $ renderToFile out $
            T.aboutPage
                (T.defaultLayout (cd & dataCss .~ aboutCss) (a ^. fileMeta)) cd a


    -- Privacy page
    siteDir </> "privacy" </> indexHtml %> \out -> do
        cd <- commonData Anything
        privacy <- posts "privacy.md"

        need [siteDir </> "css/service.css"]
        serviceCss <- liftIO $ readFile $ siteDir </> "css/service.css"

        putNormal $ "Writing page " ++ out
        let a = privacy & fileMeta %~ postUrl .~ domain ++ "/"
                        & fileMeta %~ postMeta .~
                          toMetadata ServicePage
                            { _servicePageHeadline = "Privacy policy"
                            , _servicePageCopyrightHolder = copyrightHolder
                            , _servicePageCopyrightYear = copyrightYear
                            }
        liftIO $ renderToFile out $
            T.servicePage
                (T.defaultLayout (cd & dataCss .~ serviceCss) (a ^. fileMeta)) cd a


    -- Map pages
    siteDir </> "map" </> indexHtml %> \out -> do
        cd <- commonData Anything

        need [siteDir </> "data/map.json", siteDir </> "css/map.css"]
        mapCss <- liftIO $ readFile $ siteDir </> "css/map.css"

        let meta = def
                { _postTitle = "Путешествия"
                , _postUrl = domain ++ "/map/"
                , _postMeta  = toMetadata WebPage
                      { _webPageHeadline = "Путешествия"
                      , _webPageCopyrightHolder = copyrightHolder
                      , _webPageCopyrightYear = copyrightYear
                      }
                }

        putNormal $ "Writing page " ++ out
        liftIO $ renderToFile out $
            T.mapPage (T.mapLayout (cd & dataCss .~ mapCss) meta)

    siteDir </> "map/list" </> indexHtml %> \out -> do
        cd <- commonData Anything

        need [siteDir </> "data/map.json", siteDir </> "css/maplist.css"]
        mapListCss <- liftIO $ readFile $ siteDir </> "css/maplist.css"

        mapJson <- liftIO $ LBS.readFile "data/map.json"
        let res = A.eitherDecode mapJson :: Either String MapCountries
        let meta = def
                { _postTitle = "Путешествия"
                , _postUrl = domain ++ "/map/list/"
                , _postMeta  = toMetadata WebPage
                      { _webPageHeadline = "Путешествия"
                      , _webPageCopyrightHolder = copyrightHolder
                      , _webPageCopyrightYear = copyrightYear
                      }
                }

        either error (\countries -> do
            putNormal $ "Writing page " ++ out
            liftIO $ renderToFile out $
                T.mapListPage
                    (T.defaultLayout (cd & dataCss .~ mapListCss) meta) countries
            ) res


    -- 404 error
    forM_ [siteDir </> "404" </> indexHtml, siteDir </> "404.html"] $
        \pattern -> pattern %> \out -> do
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
            , def { _suLoc = "/privacy/", _suPriority = "0.5" }
            ] ++ postsFilePaths


    -- Parse Markdown with metadata and save to temp file
    pandocCacheDir <//> "*.md" %> \out -> do
        let src = dropDirectory2 out
        putNormal $ "Reading post " ++ src
        need [src]
        file <- liftIO $ readFile src

        imagesContent <- images Anything

        pandoc <- liftIO $ handleError $ runPure $ readMarkdown readerOptions file
        let post = buildPost src imagesContent pandoc
        let pCover = post ^. fileMeta ^. postCover
        let color = mplus
                (pCover ^. coverColor)
                ((fmap (^. imageColor) . imageGetter imagesContent)
                    =<< (pCover ^. coverImg))

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
        dateKey (Just time)= T.pack $ formatShow iso8601Format time
        dateKey Nothing = terror "No date defined"

        getPostsForPage ps page =
            reverse [snd $ M.elemAt i ps | i <- [listLast - rangeEnd .. listLast - rangeStart]]
            where
                listLast = M.size ps - 1
                rangeStart = (page - 1) * pageSize
                rangeEnd = min (page * pageSize - 1) listLast

        getOlderPage prefix ps page
            | page * pageSize - 1 >= listLast = Nothing
            | otherwise = Just $ prefix ++ "page/" ++ tshow (page + 1) ++ "/"
            where
                listLast = M.size ps - 1

        getNewerPage prefix _ page
            | page == 1 = Nothing
            | page == 2 = Just prefix
            | otherwise = Just $ prefix ++ "page/" ++ tshow (page - 1) ++ "/"

        alterDate :: FilePath -> Maybe UTCTime -> Maybe UTCTime
        alterDate filePath Nothing = dateFromFilePath filePath
        alterDate _ date = date

        dateFromFilePath :: FilePath -> Maybe UTCTime
        dateFromFilePath = parseDate . T.pack . intercalate "-" . take 3 . splitAll "-" . takeFileName

        pathsFromList :: FilePath -> FilePath -> Posts -> [FilePath]
        pathsFromList prefix suffix ps =
            (prefix </> suffix) : listFilePaths
            where
                (d,m) = M.size ps `divMod` pageSize
                listFilePaths =
                    [ prefix </> pageDir </> show p </> suffix |
                        p <- [2 .. d + (if m == 0 then 2 else 1)] ]

        imageGetter :: Images -> Text -> Maybe ImageMeta
        imageGetter imagesContent filePath =
            if "/images/" `T.isPrefixOf` filePath
                then M.lookup (T.tail filePath) imagesContent
                else Nothing

-- Build map data

{-
shp2json -n ne_10m_admin_0_countries_lakes/ne_10m_admin_0_countries_lakes.shp > countries.geojson
shp2json -n ne_10m_admin_0_map_subunits/ne_10m_admin_0_map_subunits.shp > subunits.geojson
shp2json -n ne_10m_admin_1_states_provinces_lakes/ne_10m_admin_1_states_provinces_lakes.shp > regions.geojson

ndjson-filter '(d.properties.ADM0_A3 == "FRA")' < subunits.geojson > subunits.filter.geojson
ndjson-map '(d.id = d.properties.ADM_A3 || d.properties.SU_A3 || d.properties.adm1_code, delete d.properties, d)' < subunits.filter.geojson > subunits.map.geojson

ndjson-filter '(d.properties.ADM0_A3 != "FRA" && d.properties.ADM0_A3 != "RUS" && d.properties.ADM0_A3 != "USA")' < countries.geojson > countries.filter.geojson
ndjson-map '(d.id = d.properties.ADM_A3 || d.properties.SU_A3 || d.properties.adm1_code, delete d.properties, d)' < countries.filter.geojson > countries.map.geojson

ndjson-filter '(d.properties.adm0_a3 == "RUS" || d.properties.adm0_a3 == "USA")' < regions.geojson > regions.filter.geojson
ndjson-map '(d.id = d.properties.adm1_code || d.properties.SU_A3 || d.properties.ADM_A3 || d.properties.adm0_a3, delete d.properties, d)' < regions.filter.geojson > regions.map.geojson

cat subunits.map.geojson regions.map.geojson countries.map.geojson | geostitch -n > world.geojson
geo2topo -q 1e5 -n < world.geojson > world.topojson
toposimplify -f -p 0.01 < world.topojson > world.json
-}

mapData :: Rules ()
mapData = do
    siteDir </> "data/map.json" %> \out -> do
        need [json, "data/map.json"]
        Stdout my <- command [] json
            ["-f", "data/map.json", "-0"]

        liftIO $ BS.writeFile out my

    siteDir </> "data/world.json" %> \out -> do
        let subunitsSrc = "map/ne_10m_admin_0_map_subunits/ne_10m_admin_0_map_subunits.shp"
        let countriesSrc = "map/ne_10m_admin_0_countries_lakes/ne_10m_admin_0_countries_lakes.shp"
        let regionsSrc = "map/ne_10m_admin_1_states_provinces_lakes/ne_10m_admin_1_states_provinces_lakes.shp"

        need
            [ subunitsSrc
            , countriesSrc
            , regionsSrc
            , shp2json
            , ndjsonFilter
            , ndjsonMap
            , geo2topo
            , toposimplify
            ]

        Stdout subunitsGeo <- command [] shp2json ["-n", subunitsSrc]
        Stdout regionsGeo <- command [] shp2json ["-n", regionsSrc]
        Stdout countriesGeo <- command [] shp2json ["-n", countriesSrc]

        Stdout subunitsFiltered <- command [Stdin subunitsGeo] ndjsonFilter ["(d.properties.ADM0_A3 == 'FRA')"]
        Stdout subunitsMapped <- command [Stdin subunitsFiltered] ndjsonMap ["(d.id = d.properties.adm1_code || d.properties.SU_A3 || d.properties.ADM_A3 || d.properties.adm0_a3, delete d.properties, d)"]
        Stdout regionsFiltered <- command [Stdin regionsGeo] ndjsonFilter ["(d.properties.adm0_a3 == 'RUS' || d.properties.adm0_a3 == 'USA')"]
        Stdout regionsMapped <- command [Stdin regionsFiltered] ndjsonMap ["(d.id = d.properties.adm1_code || d.properties.SU_A3 || d.properties.ADM_A3 || d.properties.adm0_a3, delete d.properties, d)"]
        Stdout countriesFiltered <- command [Stdin countriesGeo] ndjsonFilter ["(d.properties.ADM0_A3 != 'FRA' && d.properties.ADM0_A3 != 'RUS' && d.properties.ADM0_A3 != 'USA')"]
        Stdout countriesMapped <- command [Stdin countriesFiltered] ndjsonMap ["(d.id = d.properties.adm1_code || d.properties.SU_A3 || d.properties.ADM_A3 || d.properties.adm0_a3, delete d.properties, d)"]

        Stdout topo <- command [Stdin (subunitsMapped ++ regionsMapped ++ countriesMapped)] geo2topo ["-q", "1e5", "-n"]
        Stdout world <- command [Stdin topo] toposimplify ["-f", "-p", "0.01"]

        liftIO $ BS.writeFile out world

-- Build styles
styles :: Rules ()
styles = do
    siteDir </> "css/cookieconsent.css" %> \out -> do
        copyFileChanged (nodeModulesDir </> "vanilla-cookieconsent/dist/cookieconsent.css") out

    siteDir </> "css/*.css" %> \out -> do
        let src = "styles" </> dropDirectory3 out -<.> "pcss"
        files <- getDirectoryFiles "." ["styles//*"]
        need (postcss : "postcss.config.js" : files)
        cmd (FileStdout out) postcss ("-c" :: FilePath) ("postcss.config.js" :: FilePath) src


favicons :: Rules ()
favicons =
    phony "phony-favicons" $ do
        faviconsFiles <- getDirectoryFiles "." ["favicons/*"]
        forM_ faviconsFiles (\src -> do
            let out = siteDir </> dropDirectory1 src
            copyFileChanged src out)

fonts :: Rules ()
fonts =
    phony "phony-fonts" $ do
        fontsFiles <- getDirectoryFiles "." ["fonts/*"]
        forM_ fontsFiles (\src -> do
            let out = siteDir </> src
            copyFileChanged src out)

-- npm packages
npmPackages :: Rules ()
npmPackages =
    [postcss, json, shp2json, geo2topo, toposimplify, ndjsonFilter, ndjsonMap
    , uglifyJs, googleClosureCompiler] &%> \_ -> do
        need ["package.json"]
        cmd ("npm" :: FilePath) ("install" :: FilePath) ("--no-save" :: FilePath)


idFromPost :: Pandoc -> Text
idFromPost (Pandoc meta _) = maybe (terror "Post have no id") getId $ lookupMeta "id" meta
    where
        getId (MetaString s) = s
        getId s = terror $ "Post id field have wrong value: " ++ tshow s

dateFromPost :: Pandoc -> Text
dateFromPost (Pandoc meta _) = maybe (terror "Post have no date") getDate $ lookupMeta "date" meta
    where
        getDate (MetaString s) = s
        getDate s = terror $ "Post date field have wrong value: " ++ tshow s
