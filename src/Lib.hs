{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( module Binary
    , dropDirectory2
    , dropDirectory3
    , repeated
    , splitAll
    ) where

import           Binary
import           Data.List
import           Data.Maybe
import           Development.Shake.FilePath
import           Text.Regex.Posix

splitAll :: String    -- ^ Pattern
         -> String    -- ^ String to split
         -> [String]  -- ^ Result
splitAll pattern = filter (not . null) . splitAll'
  where
    splitAll' src = case listToMaybe (src =~~ pattern) of
        Nothing     -> [src]
        Just (o, l) ->
            let (before, tmp) = splitAt o src
            in before : splitAll' (drop l tmp)

-- From Unique package
-- TODO more optimal version

sg :: Ord a => [a] -> [[a]]
sg = group . sort

filterByLength :: Ord a => (Int -> Bool) -> [a] -> [[a]]
filterByLength p = filter (p . length) . sg

repeated :: Ord a => [a] -> [a]
repeated = repeatedBy (>1)

repeatedBy :: Ord a => (Int -> Bool) -> [a] -> [a]
repeatedBy p = map head . filterByLength p

dropDirectory2 :: FilePath -> FilePath
dropDirectory2 = dropDirectory1 . dropDirectory1

dropDirectory3 :: FilePath -> FilePath
dropDirectory3 = dropDirectory2 . dropDirectory1
