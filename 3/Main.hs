module Main where

import Data.List (transpose, sortBy, group, sort)
import Data.Ord (comparing)
import Control.Arrow ((***))
import Numeric (readInt)
import Data.Char (digitToInt)

main :: IO ()
main = do
  f <- readFile "input/3"
  let cols = parse f
      (gamma, epsilon) = computeGammaEpsilon cols
      result = gamma * epsilon
  print result

type Bit = Char

parse :: String -> [[Bit]]
parse = transpose . lines

computeGammaEpsilon :: [[Bit]] -> (Int, Int)
computeGammaEpsilon = (parseBinary *** parseBinary) . unzip . map commonUncommon

commonUncommon :: [Bit] -> (Bit, Bit)
commonUncommon = (\[b1, b2] -> (b1, b2))
               . map head
               . sortBy (flip $ comparing length)
               . group
               . sort

parseBinary :: [Bit] -> Int
parseBinary = fst . head . readInt 2 (`elem` "01") digitToInt
