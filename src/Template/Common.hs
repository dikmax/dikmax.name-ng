{-# LANGUAGE OverloadedStrings #-}

module Template.Common where

import Control.Lens
import Data.Text.Lazy
import Lib
import Lucid
import Types

coverToStyle :: PostCover -> Attribute
coverToStyle cover =
    style_ $ toStrict $
        maybe "" (\i -> "background-image:url(" `append` pack i `append` ");") (cover ^. coverImg)
        `append` maybe "" (\i -> "background-color:" `append` pack i `append` ";") (cover ^. coverColor)
        `append` "background-position-x:" `append` pack (cover ^. coverHCenter) `append` ";"
        `append` "background-position-y:" `append` pack (cover ^. coverVCenter)
