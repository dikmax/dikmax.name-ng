{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( module Binary
    , splitAll
    ) where

import           Binary
import           Data.Maybe
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
