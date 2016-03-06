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

postcss :: FilePath
postcss = nodeModulesBinDir </> "postcss"

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

defaultReadMoreText :: Text
defaultReadMoreText = "Читать далее..."

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

