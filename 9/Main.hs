module Main where

import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (sort, group, sortBy)

import Lib.Point

main :: IO ()
main = do
  f <- readFile "input/9"
  let ds = map (map (read . pure)) . lines $ f
      lowPoints = findLowPoints ds
      result = sum . map (+1) $ lowPoints
  print result
  let depthMap = parseAsMap f
      result2 = solve depthMap
  print result2

findLowPoints :: [[Int]] -> [Int]
findLowPoints ds =
  let above = repeat 9 : ds
      below = tail ds ++ [repeat 9]
      right_ = map ((++ [9]) . tail) ds
      left_ = map (9:) ds
  in catMaybes $ concat $ zipWith5 (zipWith5 isLowPoint) ds above below right_ left_

isLowPoint :: Int -> Int -> Int -> Int -> Int -> Maybe Int
isLowPoint d a b r l =
  if d < a && d < b && d < r && d < l
  then Just d
  else Nothing

zipWith5 :: (a -> b -> c -> d -> e -> f) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
zipWith5 _ [] _  _  _  _  = []
zipWith5 _ _  [] _  _  _  = []
zipWith5 _ _  _  [] _  _  = []
zipWith5 _ _  _  _  [] _  = []
zipWith5 _ _  _  _  _  [] = []
zipWith5 f (a:as) (b:bs) (c:cs) (d:ds) (e:es) = f a b c d e : zipWith5 f as bs cs ds es

parseAsMap :: String -> Map Point Int
parseAsMap = parseGrid (\c -> read [c])

findBasin :: Map Point Int -> Point -> Maybe Point
findBasin ds p =
  let Just d = Map.lookup p ds
      pu = up p
      pd = down p
      pl = left p
      pr = right p
      a = fromMaybe 9 $ Map.lookup pu ds
      b = fromMaybe 9 $ Map.lookup pd ds
      l = fromMaybe 9 $ Map.lookup pl ds
      r = fromMaybe 9 $ Map.lookup pr ds
      lowest = snd . head . sort $ [(a, pu), (b, pd), (l, pl), (r, pr)]
  in if d < a && d < b && d < r && d < l
     then Just p
     else if d == 9
     then Nothing
     else findBasin ds lowest -- could have memoized if too slow

up :: Point -> Point
up (x, y) = (x, y - 1)

down :: Point -> Point
down (x, y) = (x, y + 1)

left :: Point -> Point
left (x, y) = (x - 1, y)

right :: Point -> Point
right (x, y) = (x + 1, y)

solve :: Map Point Int -> Int
solve m
  = product
  . take 3
  . sortBy (flip compare)
  . map length
  . group
  . sort
  . catMaybes
  . map (findBasin m)
  . Map.keys $ m
