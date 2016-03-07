{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( ImageMeta(..)
    , PostCoverType(..)
    , PostCover(..)
    , archiveMonths
    , buildPost
    , dropDirectory2
    , dropDirectory3
    , parseDate
    , postIdToUrl
    , splitAll
    , tagToUrl
    ) where

import           BasicPrelude
import           Config
import           Control.Lens
import qualified Data.Map.Lazy              as M
import qualified Data.Text                  as T
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
                { _postId          = ""
                , _postTitle       = T.pack $
                                        getMetaString (unMeta $ pandoc ^. meta)
                                            "title"
                , _postDate        = parseDate $
                                        getMetaString (unMeta $ pandoc ^. meta)
                                            "date"
                , _postCover       = buildPostCover (pandoc ^. meta)
                , _postTags        = map T.pack $
                                        getStringsList (unMeta $ pandoc ^. meta)
                                            "tags"
                , _postCollections = map T.pack $
                                        getStringsList (unMeta $ pandoc ^. meta)
                                            "collections"
                , _postUrl         = ""
                }
            | otherwise = def
                { _postCover = buildPostCover (pandoc ^. meta)
                , _postTitle = T.pack $
                                    getMetaString' "" (unMeta $ pandoc ^. meta)
                                        "title"
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
    maybe (error $ "Key \"" ++ key ++ "\" not found") extractString' $
        M.lookup key m
    where
        extractString' :: MetaValue -> String
        extractString' = fromMaybe
            (error "String cannot be extracted for key \"" ++ key ++ "\"") .
                extractString

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
            { _coverImg     = fmap T.pack $ extractString' $ M.lookup "img" m'
            , _coverVCenter = T.pack $ fromMaybe "center" $ extractString' $
                                M.lookup "vcenter" m'
            , _coverHCenter = T.pack $ fromMaybe "center" $ extractString' $
                                M.lookup "hcenter" m'
            , _coverColor   = fmap T.pack $ extractString' $ M.lookup "color" m'
            }
        extractString' :: Maybe MetaValue -> Maybe String
        extractString' (Just v) = extractString v
        extractString' Nothing = Nothing

extractString :: MetaValue -> Maybe String
extractString (MetaString str) = Just str
extractString (MetaInlines inlines) = Just $ concatMap stringify inlines
extractString _ = Nothing

-- Conversions

tagToUrl :: Text -> Text
tagToUrl tag = "/tag/" ++ tag ++ "/"

postIdToUrl :: Text -> Text
postIdToUrl pid = "/post/" ++ pid ++ "/"

archiveMonths :: [File] -> M.Map Text [File]
archiveMonths files =
    M.fromListWith (++) $ mapMaybe m1 files
    where
        m1 f =
            case f ^. fileMeta ^?! postDate of
                Just time -> Just (T.pack $ formatTime timeLocale "%Y%m" time, [f])
                Nothing -> Nothing
