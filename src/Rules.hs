module Rules where

import           BasicPrelude
import           Config
import qualified Data.ByteString            as BS
import           Development.Shake
import           Development.Shake.FilePath
import           Lib
import           System.Directory           (createDirectoryIfMissing)
import           System.Exit

clean :: Rules ()
clean = do
    phony "clean" $ do
        putNormal $ "Cleaning files in " ++ siteDir
        removeFilesAfter siteDir ["//*"]

    phony "full-clean" $ do
        putNormal $ "Cleaning files in " ++ buildDir
        removeFilesAfter buildDir ["//*"]

deploy :: Rules ()
deploy =
    phony "deploy" $ do
        need ["build"]
        {-
        need ["compress"]
        command_ [] "rsync" ["--recursive", "--delete", "--force", "--compress",
            "--progress", "--delay-updates", "--iconv=UTF8-MAC,UTF-8",
            "_build/site/", "dikmax@dikmax.name:/home/dikmax/dikmax.name/"]
        -}
        command_ [] "firebase" ["deploy", "--only", "hosting"]

prerequisites :: Rules ()
prerequisites =
    phony "prerequisites" $ do
        putNormal "Checking prerequisites"
        mapM_ check ["node", "npm", "firebase", "zopflipng", "brotli", "guetzli"]
    where
        check executable = do
            Exit code <- cmd (EchoStdout False) ("which" :: FilePath) executable
            when (code /= ExitSuccess) $ error $ "PREREQUISITE: '" ++
                executable ++ "' is not available"

demos :: Rules ()
demos =
    phony "phony-demos" $ do
        command_ [] "git" ["submodule", "update"]
        files <- getDirectoryFiles "demos" ["//*"]
        forM_ files $ \file -> do
            exists <- doesFileExist (demosDir </> file)
            unless exists $ do
                liftIO $ createDirectoryIfMissing True $ takeDirectory (demosDir </> file)
                putNormal $ "Copying file " ++ (demosDir </> file)
                copyFileChanged ("demos" </> file) (demosDir </> file)

robotsTxt :: Rules ()
robotsTxt =
    siteDir </> "robots.txt" %> \out -> do
        let src = dropDirectory2 out
        copyFileChanged src out

-- Build scripts
scripts :: Rules ()
scripts = do
    highlightJsMin %> \_ -> do
        need ["scripts/highlight.js/package.json"]
        command_ [Cwd "scripts/highlight.js/"] "npm" ["install", "--no-save"]
        command_ [Cwd "scripts/highlight.js/"] "node" ("tools/build.js" : "-t" :
            "browser" : includeHighlightingLanguages)

    proj4JsPack %> \_ -> do
        need ["scripts/proj4js/package.json"]
        command_ [Cwd "scripts/proj4js/"] "npm" ["install", "--no-save"]
        command_ [Cwd "scripts/proj4js/"] "node" ["./node_modules/.bin/grunt", "build:moll"]

    siteDir </> "scripts/main.js" %> \out -> do
        files <- getDirectoryFiles "." ["scripts//*"]
        need (postcss : highlightJsMin : files)
        io <- compressScriptSimple intersectionObserver
        h <- liftIO $ BS.readFile highlightJsMin
        my <- buildScript True False
        liftIO $ BS.writeFile out (io ++ h ++ my)

    siteDir </> "scripts/map.js" %> \out -> do
        files <- getDirectoryFiles "." ["scripts//*"]
        need (proj4JsPack : files)
        l <- liftIO $ BS.readFile leaflet
        eb <- compressUglifyJs easyButton
        p <- liftIO $ BS.readFile proj4JsPack
        pl <- compressUglifyJs proj4leaflet
        gh <- compressUglifyJs greinerHormann
        t <- liftIO $ BS.readFile topojsonLib
        my <- buildScript False True
        liftIO $ BS.writeFile out (l ++ "\n" ++ eb ++ p ++ pl ++ gh ++ t ++ my)

    where
        easyButton :: FilePath
        easyButton = nodeModulesDir </> "leaflet-easybutton/src/easy-button.js"

        highlightJsMin :: FilePath
        highlightJsMin = "scripts/highlight.js/build/highlight.min.js"

        proj4JsPack :: FilePath
        proj4JsPack = "scripts/proj4js/dist/proj4.js"

        greinerHormann :: FilePath
        greinerHormann = nodeModulesDir </> "greiner-hormann/dist/greiner-hormann.js"

        intersectionObserver :: FilePath
        intersectionObserver = nodeModulesDir </> "intersection-observer/intersection-observer.js"

        leaflet :: FilePath
        leaflet = nodeModulesDir </> "leaflet/dist/leaflet.js"

        proj4leaflet :: FilePath
        proj4leaflet = nodeModulesDir </> "proj4leaflet/src/proj4leaflet.js"

        topojsonLib :: FilePath
        topojsonLib = nodeModulesDir </> "topojson/dist/topojson.min.js"

compressScriptSimple :: FilePath -> Action ByteString
compressScriptSimple path = do
    need [googleClosureCompiler]
    Stdout my <- command [] googleClosureCompiler
        [ "--compilation_level", "SIMPLE_OPTIMIZATIONS"
        , "--warning_level", "VERBOSE"
        , "--js", path]

    return my


compressUglifyJs :: FilePath -> Action ByteString
compressUglifyJs path = do
    need [uglifyJs]
    Stdout my <- command [] uglifyJs
        [ "--compress", "--mangle", "--"
        , path
        ]
    return my

compressScriptWhitespaceOnly :: FilePath -> Action ByteString
compressScriptWhitespaceOnly path = do
    need [googleClosureCompiler]
    Stdout my <- command [] googleClosureCompiler
        [ "--compilation_level", "WHITESPACE_ONLY"
        , "--warning_level", "VERBOSE"
        , "--js", path]

    return my

buildScript :: Bool -> Bool -> Action ByteString
buildScript dHighlightJs dMap = do
    need [googleClosureCompiler]
    Stdout my <- command [] googleClosureCompiler
        ([ "--entry_point", "scripts/main.js"
        , "--dependency_mode", "PRUNE"
        , "--compilation_level", "ADVANCED_OPTIMIZATIONS"
        , "--warning_level", "VERBOSE"
        , "--language_in", "ECMASCRIPT_NEXT"
        , "--language_out", "STABLE"
        , "--module_resolution", "NODE"
        {-, "--new_type_inf"-}] ++
        defines ++
        [ "--externs", "scripts/externs/highlight.js"
        , "--externs", "scripts/externs/leaflet.js"
        , "--externs", "scripts/externs/greinerHormann.js"
        , "--externs", "scripts/externs/topojson.js"
        , "--js", "node_modules/google-closure-library/closure/goog/**.js"
        , "--js", "!node_modules/google-closure-library/closure/goog/**_test.js"
        , "--js", "node_modules/google-closure-library/third_party/closure/goog/mochikit/async/**.js"
        , "--js", "!node_modules/google-closure-library/third_party/closure/goog/mochikit/async/**_test.js"
        -- , "--js", "node_modules/leaflet/dist/leaflet-src.js"
        , "--js", "scripts/dikmax/*.js"
        , "--js", "scripts/main.js"])

    return my

    where
        defines =
            (if dHighlightJs then ["--define", "HIGHLIGHT_JS"] else []) ++
            (if dMap then ["--define", "MAP"] else [])
