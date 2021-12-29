module Main where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromJust)
import Control.Arrow ((&&&))
import Data.List (group, sort, genericLength)

main :: IO ()
main = do
  f <- readFile "input/21"
  let st = parse f
      result = answer $ playUntilWin st
  print result
  let result2 = fromJust $ Map.lookup (to2 st) allStates
  print (max (wins1 result2) (wins2 result2))

data State = State
  { pos1 :: Int
  , pos2 :: Int
  , score1 :: Int
  , score2 :: Int
  , die :: [Int]
  , rolls :: Int
  } deriving (Show, Eq)

parse :: String -> State
parse str =
  let [l1, l2] = lines str
  in State (read . pure . last $ l1) (read . pure . last $ l2) 0 0 (cycle [1..100]) 0

step :: State -> State
step st =
  let pos1' = move (sum . take 3 . die $ st) (pos1 st)
      score1' = score1 st + pos1'
      die' = drop 3 (die st)
      pos2' = move (sum . take 3 $ die') (pos2 st)
      score2' = score2 st + pos2'
  in if score1' >= 1000
     then State pos1' (pos2 st) score1' (score2 st) die' (rolls st + 3)
     else State pos1' pos2' score1' score2' (drop 3 die') (rolls st + 6)

move :: Int -> Int -> Int
move steps pos =
  (pos + steps - 1) `mod` 10 + 1

playUntilWin :: State -> State
playUntilWin st =
  let st' = step st
  in if isWin st' then st' else playUntilWin st'

isWin :: State -> Bool
isWin st = score1 st >= 1000 || score2 st >= 1000

answer :: State -> Int
answer st =
  let losingScore = if score1 st >= 1000 then score2 st else score1 st
  in losingScore * rolls st

data State2 = State2
  { pos1_ :: Int
  , pos2_ :: Int
  , score1_ :: Int
  , score2_ :: Int
  , turn1 :: Bool
  } deriving (Eq, Ord, Show)

to2 :: State -> State2
to2 st = State2 (pos1 st) (pos2 st) (score1 st) (score2 st) True

step2 :: Int -> State2 -> State2
step2 mv st =
  if turn1 st
  then
    let pos1' = move mv (pos1_ st)
        score1' = score1_ st + pos1'
    in st { pos1_ = pos1'
          , score1_ = score1'
          , turn1 = not (turn1 st)
          }
  else
    let pos2' = move mv (pos2_ st)
        score2' = score2_ st + pos2'
    in st { pos2_ = pos2'
          , score2_ = score2'
          , turn1 = not (turn1 st)
          }

data Result = Result
  { wins1 :: Integer
  , wins2 :: Integer
  } deriving (Show, Eq)

instance Monoid Result where
  mempty = Result 0 0

instance Semigroup Result where
  Result w1 w2 <> Result w1' w2' = Result (w1 + w1') (w2 + w2')

allStates :: Map State2 Result
allStates =
  let m = Map.fromList [ (st, result st) | p1 <- [1..10], p2 <- [1..10], s1 <- [0..30], s2 <- [0..30], t1 <- [True, False], let st = State2 p1 p2 s1 s2 t1]
      result st =
        if score1_ st >= 21
        then Result 1 0
        else if score2_ st >= 21
        then Result 0 1
        else
          foldMap (\(s, c) -> scale c . fromJust . flip Map.lookup m $ s)
            [ (st', c) | (r, c) <- distribution , let st' = step2 r st ]
  in m

scale :: Integer -> Result -> Result
scale c (Result w1 w2) = Result (w1 * c) (w2 * c)

distribution :: [(Int, Integer)] -- sum, count
distribution = map (head &&& genericLength) . group . sort $ [ r1 + r2 + r3 | r1 <- [1..3], r2 <- [1..3], r3 <- [1..3] ]
