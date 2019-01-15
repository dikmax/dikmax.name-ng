module Config where

import           BasicPrelude
import           Data.Time
import           Development.Shake

buildDir :: FilePath
buildDir = "_build"

pandocCacheDir :: FilePath
pandocCacheDir = buildDir </> "pandoc"

imagesBuildDir :: FilePath
imagesBuildDir = buildDir </> "images"

shakeBuildDir :: FilePath
shakeBuildDir = buildDir </> "shake"

siteDir :: FilePath
siteDir = buildDir </> "site"

brotliCacheDir :: FilePath
brotliCacheDir = buildDir </> "brotli"

zopfliCacheDir :: FilePath
zopfliCacheDir = buildDir </> "zopfli"

webpCacheDir :: FilePath
webpCacheDir = buildDir </> "webp"

sitePostsDir :: FilePath
sitePostsDir = siteDir </> "post"

pageDir :: FilePath
pageDir = "page"

tagDir :: FilePath
tagDir = "tag"

ampDir :: FilePath
ampDir = "amp"

indexHtml :: FilePath
indexHtml = "index.html"

nodeModulesDir :: FilePath
nodeModulesDir = "node_modules"

nodeModulesBinDir :: FilePath
nodeModulesBinDir = nodeModulesDir </> ".bin"

imagesDir :: FilePath
imagesDir = "images"

demosDir :: FilePath
demosDir = siteDir </> "demos"

postcss :: FilePath
postcss = nodeModulesBinDir </> "postcss"

shp2json :: FilePath
shp2json = nodeModulesBinDir </> "shp2json"

geo2topo :: FilePath
geo2topo = nodeModulesBinDir </> "geo2topo"

toposimplify :: FilePath
toposimplify = nodeModulesBinDir </> "toposimplify"

ndjsonFilter :: FilePath
ndjsonFilter = nodeModulesBinDir </> "ndjson-filter"

ndjsonMap :: FilePath
ndjsonMap = nodeModulesBinDir </> "ndjson-map"

json :: FilePath
json = nodeModulesBinDir </> "json"

uglifyJs :: FilePath
uglifyJs = nodeModulesBinDir </> "uglifyjs"

pageSize :: Int
pageSize = 5

options :: ShakeOptions
options = shakeOptions
    { shakeFiles    = shakeBuildDir
    , shakeThreads  = 0
    , shakeTimings  = True
    , shakeProgress = progressSimple
    }

imagesPatterns :: [FilePath]
imagesPatterns = [imagesDir <//> "*.png", imagesDir <//> "*.jpg", imagesDir <//> "*.gif"]

rssFeedFile :: Text
rssFeedFile = "feed.rss"

domain :: Text
domain = "https://dikmax.name"

postUrlFromId :: Text -> Text
postUrlFromId postId = "/post/" ++ postId ++ "/"

defaultReadMoreText :: Text
defaultReadMoreText = "Читать далее..."

includeHighlightingLanguages :: [String]
includeHighlightingLanguages = ["bash", "css", "haskell", "javascript",
    "markdown", "sql", "xml", "dart"]

timeLocale :: TimeLocale
timeLocale = defaultTimeLocale
  { wDays =
    [ ("Воскресенье", "вс")
    , ("Понедельник", "пн")
    , ("Вторник", "вт")
    , ("Среда", "ср")
    , ("Четверг", "чт")
    , ("Пятница", "пт")
    , ("Суббота", "сб")
    ]
  , months =
    [ ("января", "янв")
    , ("февраля", "фев")
    , ("марта", "мар")
    , ("апреля", "апр")
    , ("мая", "май")
    , ("июня", "июн")
    , ("июля", "июл")
    , ("августа", "авг")
    , ("сентября", "сен")
    , ("октября", "окт")
    , ("ноября", "ноя")
    , ("декабря", "дек")
    ]
  }

archiveTimeLocale :: TimeLocale
archiveTimeLocale = timeLocale
    { months =
      [ ("Январь", "янв")
      , ("Февраль", "фев")
      , ("Март", "мар")
      , ("Апрель", "апр")
      , ("Май", "май")
      , ("Июнь", "июн")
      , ("Июль", "июл")
      , ("Август", "авг")
      , ("Сентябрь", "сен")
      , ("Октябрь", "окт")
      , ("Ноябрь", "ноя")
      , ("Декабрь", "дек")
      ]
    }

googleAnalyticsUA :: Text
googleAnalyticsUA = "UA-32213724-1"

copyrightYear :: Int
copyrightYear = 2019
