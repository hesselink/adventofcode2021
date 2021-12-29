module Lib.Point where

import Data.Map (Map)
import qualified Data.Map as Map

type Point = (Int, Int)

parseGrid :: (Char -> a) -> String -> Map Point a
parseGrid parse
  = Map.fromList
  . concat
  . zipWith (\y -> zipWith (\x c -> ((x, y), parse c)) [0..]) [0..]
  . lines

