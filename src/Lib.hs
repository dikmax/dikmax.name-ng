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
    , idFromSrcFilePath
    , idFromDestFilePath
    , tagAndPageFromDestFilePath
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
import           Text.Regex.Posix ((=~~), (=~))
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
                { _postId            = fromMaybe "" (idFromSrcFilePath src)
                , _postMeta          = m'
                , _postTitle         = getMetaText
                                            (unMeta $ pandoc ^. meta)
                                            "title"
                , _postDate          = parseDate $ getMetaText
                                            (unMeta $ pandoc ^. meta)
                                            "date"
                , _postCover         = buildPostCover (pandoc ^. meta)
                , _postTags          = getTextsList
                                            (unMeta $ pandoc ^. meta)
                                            "tags"
                , _postCollections   = getTextsList
                                            (unMeta $ pandoc ^. meta)
                                            "collections"
                , _postFigureNumbers = getMetaBool True
                                            (unMeta $ pandoc ^. meta)
                                            "figure-numbers"
                , _postFigureResponsive = getMetaBool True
                                            (unMeta $ pandoc ^. meta)
                                            "figure-responsive"
                , _postUrl           = url
                }
            | otherwise = PageMeta
                { _postCover = buildPostCover (pandoc ^. meta)
                , _postMeta  = toMetadata WebPage
                    { _webPageHeadline = getMetaText' ""
                                (unMeta $ pandoc ^. meta) "title"
                    , _webPageCopyrightHolder = copyrightHolder
                    , _webPageCopyrightYear = copyrightYear
                    }
                , _postTitle = getMetaText' "" (unMeta $ pandoc ^. meta)
                                        "title"
                , _postTags  = getTextsList (unMeta $ pandoc ^. meta)
                                        "tags"
                , _postUrl = url
                }

        url :: Text
        url = maybe "" (\i -> domain ++ "/post/" ++ i ++ "/") (idFromSrcFilePath src)

        m' :: Metadata
        m' = toMetadata BlogPosting
            { _blogPostingHeadline =
                getMetaText (unMeta $ pandoc ^. meta) "title"
            , _blogPostingDatePublished = fromMaybe (terror "date not defined") $
                parseDate $ getMetaText (unMeta $ pandoc ^. meta) "date"
            , _blogPostingDateModified =
                parseDate $ getMetaText' "" (unMeta $ pandoc ^. meta) "modified"
            , _blogPostingAuthor = author
            , _blogPostingImage = img
            , _blogPostingPublisher = publisher
            , _blogPostingMainEntityOfPage = url
            , _blogPostingCopyrightHolder = copyrightHolder
            , _blogPostingCopyrightYear = copyrightYear
            , _blogPostingKeywords = T.intercalate ", " $ getTextsList
                 (unMeta $ pandoc ^. meta)
                 "tags"
            , _blogPostingEditor = editor
            }

        author :: Person
        author = Person -- TODO more data to person
            { _personName =
                getMetaText' "Максим Дикун" (unMeta $ pandoc ^. meta) "author"
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

        publisher :: Organization
        publisher = Organization
            { _organizationName = "[dikmax's blog]"
            , _organizationLogo = ImageObject -- TODO create logo
                { _imageObjectUrl = domain ++ "/publisher_logo.png"
                , _imageObjectWidth = 429
                , _imageObjectHeight = 60
                }
            }

        defImg :: ImageObject
        defImg = ImageObject
            { _imageObjectUrl = domain ++ "/android-chrome-192x192.png"
            , _imageObjectWidth = 192
            , _imageObjectHeight = 192
            }

parseDate :: Text -> Maybe UTCTime
parseDate str =
    parseDate' formats
    where
        parseDate' :: [String] -> Maybe UTCTime
        parseDate' [] = Nothing
        parseDate' (f : fs) =
            case parseTimeM True timeLocale f (T.unpack str) of
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

getMetaText :: M.Map Text MetaValue -> Text -> Text
getMetaText m key =
    maybe (terror $ "Key \"" ++ key ++ "\" not found") extractText' $
        M.lookup key m
    where
        extractText' :: MetaValue -> Text
        extractText' = fromMaybe
            (terror "String cannot be extracted for key \"" ++ key ++ "\"") .
                extractText

getMetaText' :: Text -> M.Map Text MetaValue -> Text -> Text
getMetaText' d m key =
    maybe d extractText' $ M.lookup key m
    where
        extractText' :: MetaValue -> Text
        extractText' = fromMaybe d . extractText


getTextsList :: M.Map Text MetaValue -> Text -> [Text]
getTextsList m key =
    maybe [] extractList $ M.lookup key m
    where
        extractList :: MetaValue -> [Text]
        extractList (MetaList values) = mapMaybe extractText values
        extractList _ = []


getMetaBool :: Bool -> M.Map Text MetaValue -> Text -> Bool
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
            { _coverImg     = extractText' $ M.lookup "img" m'
            , _coverVCenter = fromMaybe "center" $ extractText' $
                                M.lookup "vcenter" m'
            , _coverHCenter = fromMaybe "center" $ extractText' $
                                M.lookup "hcenter" m'
            , _coverSmall   = extractBool' $ M.lookup "small" m'
            , _coverColor   = extractText' $ M.lookup "color" m'
            }
        extractText' :: Maybe MetaValue -> Maybe Text
        extractText' (Just v) = extractText v
        extractText' Nothing = Nothing

        extractBool' :: Maybe MetaValue -> Bool
        extractBool' (Just v) = fromMaybe False (extractBool v)
        extractBool' Nothing = False

extractText :: MetaValue -> Maybe Text
extractText (MetaString str) = Just str
extractText (MetaInlines inlines) = Just $ T.concat $ map (stringify) inlines
extractText _ = Nothing

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

idFromSrcFilePath :: FilePath -> Maybe Text
idFromSrcFilePath filePath =
    case filePath =~ pat :: (String, String, String, [String]) of
        (_, _, _, [v]) -> Just $ T.pack v
        _              -> Nothing
    where
        pat :: String
        pat = "/[0-9]{4}/[0-9]{4}-[0-9]{2}-[0-9]{2}-(.*)\\.md$"

idFromDestFilePath :: FilePath -> Text
idFromDestFilePath filePath =
    case filePath =~ pat :: (String, String, String, [String]) of
        (_, _, _, v : _) -> T.pack v
        _              -> error $ "Can't extract id from " ++ filePath
    where
        pat :: String
        pat = "/([^/]*)(/amp)?/index\\.html$"

tagAndPageFromDestFilePath :: FilePath -> (Text, Int)
tagAndPageFromDestFilePath filePath =
    case filePath =~ pat :: (String, String, String, [String]) of
        (_, _, _, [tag, page]) -> (T.pack tag, read $ T.pack page)
        _              -> error $ "Can't extract id from " ++ filePath
    where
        pat = tagDir ++ "/([^/]*)/" ++ pageDir ++ "/([^/]*)/index.html$"
