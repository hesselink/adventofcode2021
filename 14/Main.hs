module Main where

import qualified Data.Map as Map
import Data.List (foldl')

import Lib
import Lib.Count

main :: IO ()
main = do
  f <- readFile "input/14"
  let st = parse f
      result = solve 10 st
  print result
  let result2 = solve 40 st
  print result2

solve :: Int -> State -> Integer
solve n st =
  let finalCounts = Map.elems . countElements . nTimes n step $ st
  in maximum finalCounts - minimum finalCounts

data State = State
  { counts :: Count Pair
  , rules :: [Rule]
  , initEl :: Char
  , lastEl :: Char
  } deriving (Show, Eq)

type Pair = (Char, Char) -- pair of elements AB
type Rule = (Pair, (Pair, Pair))

parse :: String -> State
parse str = case lines str of
  (tmpl:"":rls) -> State (parseTemplate tmpl) (parseRules rls) (head tmpl) (last tmpl)
  _ -> error "Failed to parse"

parseTemplate :: String -> Count Pair
parseTemplate str = count $ zipWith (,) str (tail str)

parseRules :: [String] -> [Rule]
parseRules = map parseRule

parseRule :: String -> (Pair, (Pair, Pair))
parseRule (a:b:' ':'-':'>':' ':c:[]) = ((a, b), ((a, c), (c, b)))
parseRule s = error $ "Failed to parse Rule: " ++ s

step :: State -> State
step st =
  let cs = counts st
      rs = rules st
      newCs = map (split cs) rs
      withoutChanged = foldl' (flip resetCount) cs (map fst rs)
  in st { counts = unionsCounts (withoutChanged:newCs) }

split :: Count Pair -> Rule -> Count Pair
split cs (p, (p1, p2)) =
  let c = getCount p cs
  in if c == 0 then Map.empty else fromCounts [(p1, c), (p2, c)]

countElements :: State -> Count Char
countElements st
  = fmap (`div` 2)
  . incCount (lastEl st)
  . incCount (initEl st)
  . unionsCounts
  . map (\((a,b), n) -> fromCounts [(a, n), (b, n)])
  . Map.toList
  . counts
  $ st
