module Media (mediaRules) where

import           BasicPrelude
import qualified Codec.Picture                   as P
import qualified Codec.Picture.Extra             as P
import           Config
import           Control.Lens
import qualified Data.Binary                     as B
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Base64          as BS
import qualified Data.Text                       as T
import           Development.Shake
import           Development.Shake.Config
import           Development.Shake.FilePath
import           Lib
import           Numeric
import           System.Directory                (createDirectoryIfMissing)
import           Text.Regex.Posix
import           Types


-- Build media
mediaRules :: Rules ()
mediaRules = do
    phony "sync-media" $ do
        putNormal "Syncing media"
        srcMediaDir <- getConfig "MEDIA_DIR"
        when (isJust srcMediaDir) $ do
            let dir = fromMaybe "" srcMediaDir
            files <- getDirectoryFiles dir ["//*"]
            -- TODO delete no more existent files
            forM_ files (\file -> do
                exists <- doesFileExist (mediaDir </> file)
                unless (exists || isDuplicateMedia file) $ do
                    liftIO $ createDirectoryIfMissing True $ takeDirectory (mediaDir </> file)
                    putNormal $ "Copying file " ++ (mediaDir </> file)
                    copyFileChanged (dir </> file) (mediaDir </> file)
                )

    phony "phony-media" $ do
        need ["sync-media"]
        mediaFiles <- getDirectoryFiles "." mediaPatterns
        need [siteDir </> x | x <- mediaFiles]

    forM_ mediaPatterns buildStatic

-- Check for Google Drive duplicates
isDuplicateMedia :: FilePath -> Bool
isDuplicateMedia = (=~ (" \\([0-9]+\\)\\.[a-zA-Z0-9]+$" :: String))
