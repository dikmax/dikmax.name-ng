module Config where

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

sitePagesDir :: String
sitePagesDir = siteDir </> "page"

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

defaultReadMoreText :: String
defaultReadMoreText = "Читать далее..."
