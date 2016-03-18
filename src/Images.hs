module Images where

import           BasicPrelude
import           Control.Monad.Trans.Resource    (release)
import qualified Data.Text                       as T
import           Data.Vector.Storable            ((!))
import           Development.Shake
import           Graphics.ImageMagick.MagickWand
import           Numeric
import           Types

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
