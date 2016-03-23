module Images where

import           BasicPrelude
import           Config
import           Control.Monad.Trans.Resource    (release)
import qualified Data.Binary                     as B
import qualified Data.Text                       as T
import           Data.Vector.Storable            ((!))
import           Development.Shake
import           Development.Shake.Config
import           Development.Shake.FilePath
import           Graphics.ImageMagick.MagickWand
import           Lib
import           Numeric
import           System.Directory                (createDirectoryIfMissing)

getImageMeta :: FilePath -> Action ImageMeta
getImageMeta path =
    liftIO $ withMagickWandGenesis $ do
        (_, w1) <- magickWand
        -- Read the image
        readImage w1 $ T.pack path
        width <- getImageWidth w1
        height <- getImageHeight w1

        resizeImage w1 1 1 triangleFilter 1

        (iterator_key,iterator) <- pixelIterator w1
        pixelRows <- pixelIterateList iterator
        color <- getMagickColor $ head pixelRows ! 0 -- TODO css string
        red <- getPixelRed color
        green <- getPixelGreen color
        blue <- getPixelBlue color
        release iterator_key

        return ImageMeta
            { _imageColor = (T.pack . showChar '#' . hex (truncate $ red / 256 :: Integer)
                . hex (truncate $ green / 256 :: Integer)
                . hex (truncate $ blue / 256 :: Integer)) ""
            , _imageWidth = width
            , _imageHeight = height
            }
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
                unless exists $ do
                    liftIO $ createDirectoryIfMissing True $ takeDirectory (imagesDir </> file)
                    putNormal $ "Copying file " ++ (imagesDir </> file)
                    copyFileChanged (dir </> file) (imagesDir </> file)
                )

    phony "images" $ do
        imageFiles <- getDirectoryFiles "." imagesPatterns
        need [siteDir </> x | x <- imageFiles]

    forM_ imagesPatterns buildStatic
