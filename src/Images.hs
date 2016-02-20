{-# LANGUAGE OverloadedStrings #-}

module Images where

import           Control.Lens
import           Control.Monad.Trans.Resource    (release)
import qualified Data.Text                       as T
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Base64          as BS
import           Data.Vector.Storable            ((!))
import           Development.Shake
import           Graphics.ImageMagick.MagickWand
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
        resizeImage w2 16 16 triangleFilter 1

        setImageCompressionQuality w2 95
        writeImage w2 $ Just $ T.pack temp

        (iterator_key,iterator) <- pixelIterator w1
        pixelRows <- pixelIterateList iterator
        color <- getColorAsString $ head pixelRows ! 0
        release iterator_key

        return $ ImageMeta
            { _imageColor = map (toEnum . fromEnum) $ BS.unpack color
            , _imageThumbnail = ""
            , _imageWidth = width
            , _imageHeight = height
            }

    thumb <- liftIO $ BS.readFile temp
    let result = meta & imageThumbnail .~ ("data:image/jpeg;base64," ++
            (map (toEnum . fromEnum) $ BS.unpack $ BS.encode thumb))
    return result
