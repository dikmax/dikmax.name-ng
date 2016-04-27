module Rules where

import           BasicPrelude
import           Config
import           Development.Shake
import           Development.Shake.FilePath
import           Server
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
        need ["compress"]
        command_ [] "rsync" ["--recursive", "--delete", "--force", "--compress",
            "--progress", "--delay-updates", "--iconv=UTF8-MAC,UTF-8",
            "_build/site/", "dikmax@dikmax.name:/home/dikmax/dikmax.name/"]

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
        mapM_ check ["java", "node", "npm", "rsync", "zopflipng", "bro"]
    where
        check executable = do
            Exit code <- cmd (EchoStdout False) ("which" :: FilePath) executable
            when (code /= ExitSuccess) $ error $ "PREREQUISITE: '" ++
                executable ++ "' is not available"

demos :: Rules ()
demos = do
    phony "phony-demos" $ do
        command_ [] "git" ["submodule", "update"]
        files <- getDirectoryFiles "demos" ["//*"]
        forM_ files $ \file -> do
            exists <- doesFileExist (demosDir </> file)
            unless exists $ do
                liftIO $ createDirectoryIfMissing True $ takeDirectory (demosDir </> file)
                putNormal $ "Copying file " ++ (demosDir </> file)
                copyFileChanged ("demos" </> file) (demosDir </> file)
