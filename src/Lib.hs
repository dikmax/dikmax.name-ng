{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( module Binary
    , PostCoverType(..)
    , PostCover(..)
    , dropDirectory2
    , dropDirectory3
    , getPostCover
    , getPostTitle
    , getMeta
    , splitAll
    ) where

import           Binary
import           Data.Default
import           Data.List
import qualified Data.Map.Lazy              as M
import           Data.Maybe
import           Development.Shake.FilePath
import           Text.Pandoc
import           Text.Pandoc.Shared
import           Text.Regex.Posix

data PostCoverType = CoverLight | CoverDark deriving (Eq)

data PostCover = PostCover
    { coverImg     :: Maybe String
    , coverVCenter :: String
    , coverHCenter :: String
    , coverType    :: PostCoverType
    } deriving (Eq)

instance Default PostCover where
    def = PostCover
        { coverImg     = Nothing
        , coverVCenter = "center"
        , coverHCenter = "center"
        , coverType    = CoverDark
        }


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
            , coverType    = extractType $ M.lookup "background" m
            }

        extractString :: Maybe MetaValue -> Maybe String
        extractString (Just (MetaString str)) = Just str
        extractString (Just (MetaInlines inlines)) = Just $ concatMap stringify inlines
        extractString _ = Nothing

        extractType :: Maybe MetaValue -> PostCoverType
        extractType (Just (MetaString str)) =
            if str == "light" then CoverLight else CoverDark
        extractType (Just (MetaInlines inlines)) =
            if concatMap stringify inlines == "light" then CoverLight else CoverDark
        extractType _ = CoverDark

getPostTitle :: Meta -> String
getPostTitle meta =
    case lookupMeta "title" meta of
        Just (MetaString str)      -> str
        Just (MetaInlines inlines) -> concatMap stringify inlines
        _                          -> "No proper title found."

dropDirectory2 :: FilePath -> FilePath
dropDirectory2 = dropDirectory1 . dropDirectory1

dropDirectory3 :: FilePath -> FilePath
dropDirectory3 = dropDirectory2 . dropDirectory1
