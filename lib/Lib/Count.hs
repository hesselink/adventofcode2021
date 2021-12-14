module Lib.Count where

import Data.List (foldl')
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

type Count a = Map a Integer

count :: Ord a => [a] -> Count a
count = foldl' (flip incCount) Map.empty

fromCounts :: Ord a => [(a, Integer)] -> Count a
fromCounts = unionsCounts . map (Map.fromList . pure)

incCount :: Ord a => a -> Count a -> Count a
incCount v = Map.insertWith (+) v 1

getCount :: Ord a => a -> Count a -> Integer
getCount v = fromMaybe 0 . Map.lookup v

resetCount :: Ord a => a -> Count a -> Count a
resetCount = Map.delete

unionsCounts :: Ord a => [Count a] -> Count a
unionsCounts = Map.unionsWith (+)
