module Images (getImageMeta, imagesRules) where

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


getImageMeta :: FilePath -> Action ImageMeta
getImageMeta path = withTempFile $ \temp -> do
    meta <- liftIO $ do
        eimg <- P.readImage path
        let dimg = either error id eimg
        let img = P.convertRGB8 dimg
        let scaled1 = P.scaleBilinear 1 1 img
        let scaled2 = P.scaleBilinear 32 32 img

        let (P.PixelRGB8 red green blue) = P.pixelAt scaled1 0 0
        P.writePng temp scaled2

        return ImageMeta
            { _imageColor = (T.pack . showChar '#' . hex red
                . hex green
                . hex blue) ""
            , _imageThumbnail = ""
            , _imageWidth = P.imageWidth img
            , _imageHeight = P.imageHeight img
            }

    command_ [] "guetzli" [temp, temp]

    thumb <- liftIO $ BS.readFile temp
    let result = meta & imageThumbnail .~ ("data:image/jpeg;base64," ++
            T.pack (map (toEnum . fromEnum) $ BS.unpack $ BS.encode thumb))
    return result
    where
        hex a
            | a < 16 = showChar '0' . showHex a
            | otherwise = showHex a


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
                unless (exists || isDuplicateImage file) $ do
                    liftIO $ createDirectoryIfMissing True $ takeDirectory (imagesDir </> file)
                    putNormal $ "Copying file " ++ (imagesDir </> file)
                    copyFileChanged (dir </> file) (imagesDir </> file)
                )

    phony "phony-images" $ do
        imageFiles <- getDirectoryFiles "." imagesPatterns
        need [siteDir </> x | x <- imageFiles]

    forM_ imagesPatterns buildStatic

-- Check for Google Drive duplicates
isDuplicateImage :: FilePath -> Bool
isDuplicateImage = (=~ (" \\([0-9]+\\)\\.[a-zA-Z0-9]+$" :: String))
