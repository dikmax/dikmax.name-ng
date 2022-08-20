module Template.Common where

import           BasicPrelude
import           Control.Lens
import qualified Data.Text           as T
import           Lucid
import           Types

-- TODO remove -webkit- prefix when feature is fully implemented
coverToStyle :: File -> Text
coverToStyle file =
    maybe "" (\i -> "background-color:" ++ i ++ ";") (cover ^. coverColor)
    ++ maybe "" backgroundImage (cover ^. coverImg)
    ++ "background-position-x:" ++ (cover ^. coverHCenter) ++ ";"
    ++ "background-position-y:" ++ (cover ^. coverVCenter) ++ ";"
    where
        cover = file ^. fileMeta ^. postCover

        avifSource i =
            if ".jpg" `T.isSuffixOf` i && "/" `T.isPrefixOf` i then Just $ i ++ ".avif" else Nothing

        webpSource i =
            if "/" `T.isPrefixOf` i then Just $ i ++ ".webp" else Nothing

        imageSet :: Text -> Text
        imageSet i =
            maybe "" (\avif -> "url(\"" ++ avif ++ "\") type(\"image/avif\")," ) (avifSource i)
            ++ maybe "" (\webp -> "url(\"" ++ webp ++ "\") type(\"image/webp\")," ) (webpSource i)
            ++ "url(\"" ++ i ++ "\") type(\"image/jpeg\")"

        backgroundImage :: Text -> Text
        backgroundImage i =
            "background-image:url(" ++ i ++ ");"
            ++ "background-image:-webkit-image-set(" ++ imageSet i ++ ");"
            ++ "background-image:image-set(" ++ imageSet i ++ ");"
