{-# LANGUAGE ParallelListComp #-}
module Main where

import Data.List (group, sort)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  f <- readFile "input/5"
  let ls = parse f
      result = length . overlappingPoints . filter (not . isDiagonal) $ ls
  print result
  let result2 = length . overlappingPoints $ ls
  print result2

type Line = (Point, Point)
type Point = (Int, Int)

parse :: String -> [Line]
parse = map parseLine . lines

parseLine :: String -> Line
parseLine str =
  let [p1, p2] = splitOn " -> " str
  in (parsePoint p1, parsePoint p2)

parsePoint :: String -> Point
parsePoint str =
  let [x, y] = splitOn "," str
  in (read x, read y)

allPoints :: Line -> [Point]
allPoints ((x1, y1), (x2, y2)) | x1 == x2 || y1 == y2 =
  [(x, y) | x <- enumFromThenTo x1 stepX x2, y <- enumFromThenTo y1 stepY y2]
                               | otherwise = [(x, y) | x <- enumFromThenTo x1 stepX x2| y <- enumFromThenTo y1 stepY y2]
    where
      stepX = if x2 > x1 then x1 + 1 else x1 - 1
      stepY = if y2 > y1 then y1 + 1 else y1 - 1

overlappingPoints :: [Line] -> [Point]
overlappingPoints
  = map head . filter ((> 1) . length) . group . sort . concatMap allPoints

isDiagonal :: Line -> Bool
isDiagonal ((x1, y1), (x2, y2)) = not (x1 == x2 || y1 == y2)
