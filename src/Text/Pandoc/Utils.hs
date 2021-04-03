module Text.Pandoc.Utils where

import           BasicPrelude
import qualified Data.Text        as T
import           Text.Pandoc
import           Text.Regex.Posix
import           Text.Regex.Posix.String

extractTeaser :: Pandoc -> Maybe (Pandoc, Text)
extractTeaser (Pandoc m blocks) =
    first (Pandoc m) <$> extractTeaser' blocks

    where
        extractTeaser' :: [Block] -> Maybe ([Block], Text)
        extractTeaser' [] = Nothing
        extractTeaser' (b : bs) =
            case getTeaser b of
                Just str -> Just ([], T.pack str)
                Nothing -> first (b :) <$> extractTeaser' bs

        getTeaser :: Block -> Maybe String
        getTeaser (RawBlock "html" str) =
            case (T.unpack str) =~ pat :: (String, String, String, [String]) of
                (_, _, _, [""]) -> Just ""
                (_, _, _, [v]) -> Just $ tail v
                _              -> Nothing
            where
                pat :: String
                pat = "^\\s*<!--more( .*)?-->\\s*$"

        getTeaser _ = Nothing
