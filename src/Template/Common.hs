{-# LANGUAGE OverloadedStrings #-}

module Template.Common where

import Lucid
import Data.Text.Lazy
import Lib

coverToStyle :: PostCover -> Attribute
coverToStyle cover =
    style_ $ toStrict $
        maybe "" (\i -> "background-image:url(" `append` pack i `append` ");") (coverImg cover)
        `append` maybe "" (\i -> "background-color:" `append` pack i `append` ";") (coverColor cover)
        `append` "background-position-x:" `append` pack (coverHCenter cover) `append` ";"
        `append` "background-position-y:" `append` pack (coverVCenter cover)
