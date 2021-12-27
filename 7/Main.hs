module Main where

import Data.List.Split (splitOn)
import Data.List (sort, group)
import Control.Arrow ((&&&))

import Lib

main :: IO ()
main = do
  f <- readFile "input/7"
  let ns = map read . splitOn "," $ f
      crabs = map (head &&& length) . group . sort $ ns
      fuels = map (flip fuelNeeded crabs) [0..]
      minFuel = findMin fuels
  print minFuel
  let fuels2 = map (flip fuelNeeded2 crabs) [0..]
      minFuel2 = findMin fuels2
  print minFuel2

fuelNeeded :: Int -> [(Int, Int)] -> Int
fuelNeeded pos = sum . map fuelOne
  where
    fuelOne (crab, num) = abs (crab - pos) * num

findMin :: [Int] -> Int
findMin [] = error "findMin on empty list"
findMin (x:xs) = go x xs
  where
    go m [] = m
    go m (y:ys) | y < m = go y ys
                | otherwise = m

fuelNeeded2 :: Int -> [(Int, Int)] -> Int
fuelNeeded2 pos = sum . map fuelOne
  where
    fuelOne (crab, num) = triangleNum (abs (crab - pos)) * num
