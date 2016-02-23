module Template.Common where

import           BasicPrelude
import           Control.Lens
import           Lucid
import           Types

coverToStyle :: File -> Attribute
coverToStyle file =
    style_ $
        maybe "" (\i -> "background-image:url(" ++ i ++ ");") (cover ^. coverImg)
        ++ maybe "" (\i -> "background-color:" ++ i ++ ";") (cover ^. coverColor)
        ++ "background-position-x:" ++ (cover ^. coverHCenter) ++ ";"
        ++ "background-position-y:" ++ (cover ^. coverVCenter)
    where
        cover = file ^. fileMeta ^. postCover
