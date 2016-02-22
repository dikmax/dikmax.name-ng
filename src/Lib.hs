{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( ImageMeta(..)
    , PostCoverType(..)
    , PostCover(..)
    , buildPost
    , dropDirectory2
    , dropDirectory3
    , parseDate
    , splitAll
    , tagToUrl
    ) where

import           Config
import           Control.Lens
import           Data.List
import qualified Data.Map.Lazy              as M
import           Data.Maybe
import           Data.Time
import           Development.Shake.FilePath
import           Text.Pandoc
import           Text.Pandoc.Lens
import           Text.Pandoc.Shared
import           Text.Regex.Posix ((=~~))
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
buildPost src pandoc = File m pandoc
    where
        m :: FileMeta
        m
            | "posts/" `isPrefixOf` src = PostMeta
                { _postId    = ""
                , _postTitle = getMetaString (unMeta $ pandoc ^. meta) "title"
                , _postDate  = parseDate $ getMetaString (unMeta $ pandoc ^. meta) "date"
                , _postCover = buildPostCover (pandoc ^. meta)
                , _postTags  = getStringsList (unMeta $ pandoc ^. meta) "tags"
                }
            | otherwise = def
                { _postCover = buildPostCover (pandoc ^. meta)
                , _postTitle = getMetaString' "" (unMeta $ pandoc ^. meta) "title"
                }

parseDate :: String -> Maybe UTCTime
parseDate str =
    parseDate' formats
    where
        parseDate' :: [String] -> Maybe UTCTime
        parseDate' [] = Nothing
        parseDate' (f : fs) =
            case parseTimeM True timeLocale f str of
                Nothing -> parseDate' fs
                a -> a

formats :: [String]
formats    =
    [ "%Y-%m-%dT%H:%M:%S%Z"
    , "%Y-%m-%d %H:%M:%S%Z"
    , "%Y-%m-%dT%H:%M%Z"
    , "%Y-%m-%d %H:%M%Z"
    , "%Y-%m-%d"
    , "%a, %d %b %Y %H:%M:%S %Z"
    , "%B %e, %Y %l:%M %p"
    , "%B %e, %Y"
    , "%b %d, %Y"
    ]

getMetaString :: M.Map String MetaValue -> String -> String
getMetaString m key =
    maybe (error $ "Key \"" ++ key ++ "\" not found") extractString' $ M.lookup key m
    where
        extractString' :: MetaValue -> String
        extractString' = fromMaybe (error "String cannot be extracted for key \"" ++ key ++ "\"") . extractString

getMetaString' :: String -> M.Map String MetaValue -> String -> String
getMetaString' d m key =
    maybe d extractString' $ M.lookup key m
    where
        extractString' :: MetaValue -> String
        extractString' = fromMaybe d . extractString


getStringsList :: M.Map String MetaValue -> String -> [String]
getStringsList m key =
    maybe [] extractList $ M.lookup key m
    where
        extractList :: MetaValue -> [String]
        extractList (MetaList values) = mapMaybe extractString values
        extractList _ = []


buildPostCover :: Meta -> PostCover
buildPostCover m =
    case lookupMeta "cover" m of
        Just (MetaMap map') -> cover map'
        _                -> def
    where
        cover m' = PostCover
            { _coverImg     = extractString' $ M.lookup "img" m'
            , _coverVCenter = fromMaybe "center" $ extractString' $ M.lookup "vcenter" m'
            , _coverHCenter = fromMaybe "center" $ extractString' $ M.lookup "hcenter" m'
            , _coverColor   = extractString' $ M.lookup "color" m'
            }
        extractString' :: Maybe MetaValue -> Maybe String
        extractString' (Just v) = extractString v
        extractString' Nothing = Nothing

extractString :: MetaValue -> Maybe String
extractString (MetaString str) = Just str
extractString (MetaInlines inlines) = Just $ concatMap stringify inlines
extractString _ = Nothing

-- Conversions

tagToUrl :: String -> String
tagToUrl tag = "/tag/" ++ tag ++ "/"
