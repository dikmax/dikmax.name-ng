{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( ImageMeta(..)
    , PostCoverType(..)
    , PostCover(..)
    , dropDirectory2
    , dropDirectory3
    , getPostCover
    , getPostTitle
    , getMeta
    , setPostCover
    , splitAll
    ) where

import qualified Data.Map.Lazy              as M
import           Data.Maybe
import           Development.Shake.FilePath
import           Text.Pandoc
import           Text.Pandoc.Shared
import           Text.Regex.Posix
import           Types

getMeta :: Pandoc -> Meta
getMeta (Pandoc meta _) = meta

getPostCover :: Meta -> PostCover
getPostCover meta =
    case lookupMeta "cover" meta of
        Just (MetaMap m) -> cover m
        _                -> def
    where
        cover m = PostCover
            { coverImg     = extractString $ M.lookup "img" m
            , coverVCenter = fromMaybe "center" $ extractString $ M.lookup "vcenter" m
            , coverHCenter = fromMaybe "center" $ extractString $ M.lookup "hcenter" m
            , coverColor   = extractString $ M.lookup "color" m
            }

        extractString :: Maybe MetaValue -> Maybe String
        extractString (Just (MetaString str)) = Just str
        extractString (Just (MetaInlines inlines)) = Just $ concatMap stringify inlines
        extractString _ = Nothing

setPostCover :: PostCover -> MetaValue
setPostCover cover =
    MetaMap $ M.fromList $
        [("img", MetaString $ fromMaybe "" $ coverImg cover) |isJust $ coverImg cover ] ++
        [("vcenter", MetaString $ coverVCenter cover), ("hcenter", MetaString $ coverHCenter cover)] ++
        [("color", MetaString $ fromMaybe "" $ coverColor cover) | isJust $ coverColor cover]

getPostTitle :: Meta -> String
getPostTitle meta =
    case lookupMeta "title" meta of
        Just (MetaString str)      -> str
        Just (MetaInlines inlines) -> concatMap stringify inlines
        _                          -> "No proper title found."

splitAll :: String    -- ^ Pattern
         -> String    -- ^ String to split
         -> [String]  -- ^ Result
splitAll p = filter (not . null) . splitAll'
    where
        splitAll' src = case listToMaybe (src =~~ p) of
            Nothing     -> [src]
            Just (o, l) ->
                let (before, tmp) = splitAt o src
                in before : splitAll' (drop l tmp)


dropDirectory2 :: FilePath -> FilePath
dropDirectory2 = dropDirectory1 . dropDirectory1

dropDirectory3 :: FilePath -> FilePath
dropDirectory3 = dropDirectory2 . dropDirectory1
