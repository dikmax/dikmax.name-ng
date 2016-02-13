{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( ImageMeta(..)
    , PostCoverType(..)
    , PostCover(..)
    , buildPost
    , dropDirectory2
    , dropDirectory3
    , splitAll
    ) where

import           Control.Lens
import           Data.List
import qualified Data.Map.Lazy              as M
import           Data.Maybe
import           Development.Shake.FilePath
import           Text.Pandoc
import           Text.Pandoc.Lens
import           Text.Pandoc.Shared
import           Text.Regex.Posix
import           Types

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

buildPost :: FilePath -> Pandoc -> File
buildPost src pandoc = File (m src) pandoc
    where
        m :: FilePath -> FileMeta
        m src
            | "posts/" `isPrefixOf` src = PostMeta
                { _postId    = ""
                , _postTitle = getMetaString (unMeta $ pandoc ^. meta) "title"
                , _postDate  = getMetaString (unMeta $ pandoc ^. meta) "date"
                , _postCover = buildPostCover (pandoc ^. meta)
                , _postTags  = []
                }
            | otherwise = PageMeta
                { _postCover = buildPostCover (pandoc ^. meta)
                }

getMetaString :: M.Map String MetaValue -> String -> String
getMetaString m key =
    maybe (error $ "Key \"" ++ key ++ "\" not found") extractString' $ M.lookup key m
    where
        extractString' :: MetaValue -> String
        extractString' (MetaString str) = str
        extractString' (MetaInlines inlines) = concatMap stringify inlines
        extractString' _ = error "String cannot be extracted for key \"" ++ key ++ "\""


buildPostCover :: Meta -> PostCover
buildPostCover m =
    case lookupMeta "cover" m of
        Just (MetaMap map') -> cover map'
        _                -> def
    where
        cover m' = PostCover
            { _coverImg     = extractString $ M.lookup "img" m'
            , _coverVCenter = fromMaybe "center" $ extractString $ M.lookup "vcenter" m'
            , _coverHCenter = fromMaybe "center" $ extractString $ M.lookup "hcenter" m'
            , _coverColor   = extractString $ M.lookup "color" m'
            }

extractString :: Maybe MetaValue -> Maybe String
extractString (Just (MetaString str)) = Just str
extractString (Just (MetaInlines inlines)) = Just $ concatMap stringify inlines
extractString _ = Nothing
