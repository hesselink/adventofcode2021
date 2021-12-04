module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Data.List (groupBy, sortBy, intercalate)
import Data.Function (on)
import Data.Maybe (isNothing, fromMaybe)
import Data.Ord (comparing)

main :: IO ()
main = do
  f <- readFile "input/4"
  let startState = parse f
      endState = runUntilFirstBingo startState
      [winningBoard] = filter hasBingo (boards endState)
      result = score (lastDraw endState) winningBoard
  print result

data State = State
  { draws :: [Int]
  , lastDraw :: Int
  , boards :: [Board]
  } deriving (Show, Eq)

type Board = Map (Int, Int) (Maybe Int)

parse :: String -> State
parse str =
  let (hd:tl) = lines str
      ds = map read . splitOn "," $ hd
      bs = map parseBoard . filter (/= []) . splitOn [""] $ tl
  in State ds 0 bs

parseBoard :: [String] -> Board
parseBoard ls =
  let nums = map (map (read :: String -> Int) . words) ls
      mk y ns = map (mkOne y) ns
      mkOne y (x, n) = ((x, y), Just n)
  in Map.fromList $ concat $ zipWith mk [0..] (map (zip [0..]) nums)

runUntilFirstBingo :: State -> State
runUntilFirstBingo st =
  if anyHasBingo st
  then st
  else let st' = drawOne st
       in runUntilFirstBingo st'

drawOne :: State -> State
drawOne st =
  case draws st of
    [] -> st
    (d:ds) ->
      let bs = crossOff d (boards st)
      in State ds d bs

crossOff :: Int -> [Board] -> [Board]
crossOff n = map (crossOffOne n)

crossOffOne :: Int -> Board -> Board
crossOffOne n = fmap (\v -> if v == Just n then Nothing else v)

anyHasBingo :: State -> Bool
anyHasBingo = any hasBingo . boards

hasBingo :: Board -> Bool
hasBingo b =
  let l = Map.toList b
      rs = map (map snd)
         . groupBy ((==) `on` (snd . fst))
         . sortBy (comparing (snd . fst) <> comparing (fst . fst))
         $ l
      cs = map (map snd)
         . groupBy ((==) `on` (fst . fst))
         . sortBy (comparing (fst . fst) <> comparing (snd . fst))
         $ l
  in any (all isNothing) rs || any (all isNothing) cs

ppr :: State -> String
ppr st = intercalate "\n\n" $
  intercalate "," (map show (draws st))
  :
  map pprBoard (boards st)

pprBoard :: Board -> String
pprBoard = intercalate "\n"
         . map (intercalate " " . map (maybe "." showNum))
         . map (map snd)
         . groupBy ((==) `on` (snd . fst))
         . sortBy (comparing (snd . fst) <> comparing (fst . fst))
         . Map.toList

showNum :: Int -> String
showNum n | n < 10 = " " <> show n
          | otherwise = show n

score :: Int -> Board -> Int
score d = (* d) . sum . map (fromMaybe 0) . Map.elems
