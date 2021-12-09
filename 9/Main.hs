module Main where

import Data.Maybe (catMaybes)

main :: IO ()
main = do
  f <- readFile "input/9"
  let ds = map (map (read . pure)) . lines $ f
      lowPoints = findLowPoints ds
      result = sum . map (+1) $ lowPoints
  print result

findLowPoints :: [[Int]] -> [Int]
findLowPoints ds =
  let above = repeat 9 : ds
      below = tail ds ++ [repeat 9]
      right = map ((++ [9]) . tail) ds
      left = map (9:) ds
  in catMaybes $ concat $ zipWith5 (zipWith5 isLowPoint) ds above below right left

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
