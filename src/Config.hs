module Config where

import           Data.Time
import           Development.Shake
import           Development.Shake.FilePath

buildDir :: String
buildDir = "_build"

pandocCacheDir :: String
pandocCacheDir = buildDir </> "pandoc"

imagesBuildDir :: String
imagesBuildDir = buildDir </> "images"

shakeBuildDir :: String
shakeBuildDir = buildDir </> "shake"


siteDir :: String
siteDir = buildDir </> "site"

sitePostsDir :: String
sitePostsDir = siteDir </> "post"

pageDir :: String
pageDir = "page"

tagDir :: String
tagDir = "tag"

indexHtml :: String
indexHtml = "index.html"

nodeModulesDir :: String
nodeModulesDir = "node_modules"

nodeModulesBinDir :: String
nodeModulesBinDir = nodeModulesDir </> ".bin"

imagesDir :: String
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

defaultReadMoreText :: String
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
