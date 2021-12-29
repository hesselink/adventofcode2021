module Main where

main :: IO ()
main = do
  f <- readFile "input/21"
  let st = parse f
      result = answer $ playUntilWin st
  print result

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
