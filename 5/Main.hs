module Main where

import Data.List (group, sort)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  f <- readFile "input/5"
  let ls = parse f
      result = length . overlappingPoints $ ls
  print result

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
  [(x, y) | x <- [xMin .. xMax], y <- [yMin .. yMax]]
                               | otherwise = []
    where
      xMin = min x1 x2
      yMin = min y1 y2
      xMax = max x1 x2
      yMax = max y1 y2

overlappingPoints :: [Line] -> [Point]
overlappingPoints
  = map head . filter ((> 1) . length) . group . sort . concatMap allPoints
