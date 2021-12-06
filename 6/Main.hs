{-# LANGUAGE BangPatterns #-}
module Main where

import Data.List (genericLength, foldl')
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  f <- readFile "input/6"
  let ns = parse f
      result = length $ simulate 80 ns
  print result
  let result2 = simulateMemo 256 ns
  print result2

parse :: String -> [Int]
parse = map read . splitOn ","

simulate :: Int -> [Int] -> [Int]
simulate 0 xs = xs
simulate n xs = simulate (n-1) (step xs)

step :: [Int] -> [Int]
step = concatMap stepOne

stepOne :: Int -> [Int]
stepOne 0 = [6,8]
stepOne n = [n - 1]

simulateMemo :: Int -> [Int] -> Integer
simulateMemo 0 xs = genericLength xs
simulateMemo n xs = fst $ foldl' (\(s, m) x -> let !(s', m') = goOne n x m in (s + s', m')) (0, Map.empty) xs
  where
    goOne :: Int -> Int -> Map (Int, Int) Integer -> (Integer, Map (Int, Int) Integer)
    goOne 0 _ m = (1, m)
    goOne n' x m =
      case Map.lookup (n', x) m of
        Just v -> (v, m)
        Nothing -> case x of
          0 ->
            let !(v, m') = goOne (n' - 1) 6 m
                !(v', m'') = goOne (n' - 1) 8 m'
            in (v + v', Map.insert (n', x) (v + v') m'')
          _ ->
            let !(v, m') = goOne (n' - 1) (x - 1) m
            in (v, Map.insert (n', x) v m')
