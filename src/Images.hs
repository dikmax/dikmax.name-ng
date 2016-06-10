module Images where

import           BasicPrelude
import           Config
import           Control.Lens
import           Control.Monad.Trans.Resource    (release)
import qualified Data.Binary                     as B
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Base64          as BS
import qualified Data.Text                       as T
import           Data.Vector.Storable            ((!))
import           Development.Shake
import           Development.Shake.Config
import           Development.Shake.FilePath
import           Graphics.ImageMagick.MagickWand
import           Lib
import           Numeric
import           System.Directory                (createDirectoryIfMissing)
import           Types

getImageMeta :: FilePath -> Action ImageMeta
getImageMeta path = withTempFile $ \temp -> do
    meta <- liftIO $ withMagickWandGenesis $ do
        (_, w1) <- magickWand
        -- Read the image
        readImage w1 $ T.pack path
        width <- getImageWidth w1
        height <- getImageHeight w1

        (_, w2) <- cloneMagickWand w1

        resizeImage w1 1 1 triangleFilter 1
        resizeImage w2 32 32 triangleFilter 1

        (iterator_key,iterator) <- pixelIterator w1
        pixelRows <- pixelIterateList iterator
        color <- getMagickColor $ head pixelRows ! 0 -- TODO css string
        red <- getPixelRed color
        green <- getPixelGreen color
        blue <- getPixelBlue color
        release iterator_key

        setImageCompressionQuality w2 75
        writeImage w2 $ Just $ T.pack temp

        return ImageMeta
            { _imageColor = (T.pack . showChar '#' . hex (truncate $ red / 256 :: Integer)
                . hex (truncate $ green / 256 :: Integer)
                . hex (truncate $ blue / 256 :: Integer)) ""
            , _imageThumbnail = ""
            , _imageWidth = width
            , _imageHeight = height
            }

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
                unless exists $ do
                    liftIO $ createDirectoryIfMissing True $ takeDirectory (imagesDir </> file)
                    putNormal $ "Copying file " ++ (imagesDir </> file)
                    copyFileChanged (dir </> file) (imagesDir </> file)
                )

    phony "phony-images" $ do
        imageFiles <- getDirectoryFiles "." imagesPatterns
        need [siteDir </> x | x <- imageFiles]

    forM_ imagesPatterns buildStatic
