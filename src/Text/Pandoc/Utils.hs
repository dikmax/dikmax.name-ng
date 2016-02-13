{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Utils where

import           Control.Arrow
import           Text.Pandoc
import           Text.Regex.Posix

extractTeaser :: Pandoc -> Maybe (Pandoc, String)
extractTeaser (Pandoc m blocks) =
    first (Pandoc m) <$> extractTeaser' blocks

    where
        extractTeaser' :: [Block] -> Maybe ([Block], String)
        extractTeaser' [] = Nothing
        extractTeaser' (b : bs) =
            case getTeaser b of
                Just str -> Just ([], str)
                Nothing -> first (b :) <$> extractTeaser' bs

        getTeaser :: Block -> Maybe String
        getTeaser (RawBlock "html" str) =
            case str =~ pat :: (String, String, String, [String]) of
                (_, _, _, [""]) -> Just ""
                (_, _, _, [v]) -> Just $ tail v
                _              -> Nothing
            where
                pat :: String
                pat = "^\\s*<!--more( .*)?-->\\s*$"

        getTeaser _ = Nothing
