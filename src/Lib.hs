{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( ImageMeta(..)
    , PostCoverType(..)
    , PostCover(..)
    , archiveMonths
    , buildPost
    , buildStatic
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
import           Development.Shake
import           Development.Shake.FilePath
import           JsonLD
import           Text.Pandoc
import           Text.Pandoc.Lens
import           Text.Pandoc.Shared
import           Text.Regex.Posix ((=~~))
import           Types


-- Static files, that just should be copied to `siteDir`
buildStatic :: FilePath -> Rules ()
buildStatic filePath =
    siteDir </> filePath %> \out -> do
        let src = dropDirectory2 out
        -- putNormal $ "Copying file " ++ out
        copyFileChanged src out


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

buildPost :: FilePath -> Images -> Pandoc -> File
buildPost src images pandoc = File m pandoc
    where
        m :: FileMeta
        m
            | "posts/" `isPrefixOf` src = PostMeta
                { _postId            = ""
                , _postMeta          = m'
                , _postTitle         = T.pack $ getMetaString
                                            (unMeta $ pandoc ^. meta)
                                            "title"
                , _postDate          = parseDate $ getMetaString
                                            (unMeta $ pandoc ^. meta)
                                            "date"
                , _postCover         = buildPostCover (pandoc ^. meta)
                , _postTags          = map T.pack $ getStringsList
                                            (unMeta $ pandoc ^. meta)
                                            "tags"
                , _postCollections   = map T.pack $ getStringsList
                                            (unMeta $ pandoc ^. meta)
                                            "collections"
                , _postFigureNumbers = getMetaBool True
                                            (unMeta $ pandoc ^. meta)
                                            "figure-numbers"
                , _postFigureResponsive = getMetaBool True
                                            (unMeta $ pandoc ^. meta)
                                            "figure-responsive"
                , _postUrl           = ""
                }
            | otherwise = def
                { _postCover = buildPostCover (pandoc ^. meta)
                , _postTitle = T.pack $
                                    getMetaString' "" (unMeta $ pandoc ^. meta)
                                        "title"
                }

        m' :: Metadata
        m' = toMetadata $ BlogPosting
            { _blogPostingHeadline =
                T.pack $ getMetaString (unMeta $ pandoc ^. meta) "title"
            , _blogPostingDatePublished = fromMaybe (error "date not defined") $
                parseDate $ getMetaString (unMeta $ pandoc ^. meta) "date"
            , _blogPostingDateModified =
                parseDate $ getMetaString' "" (unMeta $ pandoc ^. meta) "modified"
            , _blogPostingAuthor = author
            , _blogPostingImage = img
            }

        author :: Person
        author = Person -- TODO more data to person
            { _personName = T.pack $
                getMetaString' "Maxim Dikun" (unMeta $ pandoc ^. meta) "author"
            }

        img :: ImageObject
        img =
            let cover = buildPostCover (pandoc ^. meta) in
            maybe defImg (\ci ->
                maybe defImg (\c -> ImageObject
                    { _imageObjectUrl = domain ++ ci
                    , _imageObjectWidth = c ^. imageWidth
                    , _imageObjectHeight = c ^. imageHeight
                    }) $ M.lookup (T.tail ci) images
                ) $ cover ^. coverImg

        defImg :: ImageObject
        defImg = ImageObject
            { _imageObjectUrl = domain ++ "/android-chrome-192x192.png"
            , _imageObjectWidth = 192
            , _imageObjectHeight = 192
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


getMetaBool :: Bool -> M.Map String MetaValue -> String -> Bool
getMetaBool d m key =
    maybe d extractBool' $ M.lookup key m
    where
        extractBool' :: MetaValue -> Bool
        extractBool' = fromMaybe d . extractBool


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
            , _coverSmall   = extractBool' $ M.lookup "small" m'
            , _coverColor   = fmap T.pack $ extractString' $ M.lookup "color" m'
            }
        extractString' :: Maybe MetaValue -> Maybe String
        extractString' (Just v) = extractString v
        extractString' Nothing = Nothing

        extractBool' :: Maybe MetaValue -> Bool
        extractBool' (Just v) = fromMaybe False (extractBool v)
        extractBool' Nothing = False

extractString :: MetaValue -> Maybe String
extractString (MetaString str) = Just str
extractString (MetaInlines inlines) = Just $ concatMap stringify inlines
extractString _ = Nothing

extractBool :: MetaValue -> Maybe Bool
extractBool (MetaBool b) = Just b
extractBool _ = Nothing

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
