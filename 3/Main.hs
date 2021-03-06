{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List (transpose, sortBy, group, sort, unfoldr)
import Data.Ord (comparing)
import Control.Arrow ((***))

import Lib

main :: IO ()
main = do
  f <- readFile "input/3"
  let rows = lines f
      (gamma, epsilon) = computeGammaEpsilon (transpose rows)
      result = gamma * epsilon
  print result
  let (o, c) = (oxygen rows, co2 rows)
      result2 :: Int = readBinUnsafe o * readBinUnsafe c
  print result2

type Bit = Char

computeGammaEpsilon :: [[Bit]] -> (Int, Int)
computeGammaEpsilon = (readBinUnsafe *** readBinUnsafe) . unzip . map commonUncommon

commonUncommon :: [Bit] -> (Bit, Bit)
commonUncommon = (\[b1, b2] -> (b1, b2))
               . map head
               . sortBy (flip (comparing length <> compare))
               . group
               . sort

oxygen :: [[Bit]] -> [Bit]
oxygen = unfoldr (step fst)

co2 :: [[Bit]] -> [Bit]
co2 = unfoldr (step snd)

step :: ((Bit, Bit) -> Bit) -> [[Bit]] -> Maybe (Bit, [[Bit]])
step _  [] = Nothing
step _  [[]] = Nothing
step _  [r:row] = Just (r, [row])
step cu rows =
  let d = cu . commonUncommon . map head $ rows
      left = filter ((== d) . head) rows
  in Just (d, map tail left)
