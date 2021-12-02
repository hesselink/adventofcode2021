{-# LANGUAGE LambdaCase #-}
module Main where

main :: IO ()
main = do
  f <- readFile "input/2"
  let moves = parse f
      (endH, endD) = execute (0,0) moves
      result = endH * endD
  print result
  let endState = execute2 initState moves
      result2 = horizontal endState * depth endState
  print result2

type Pos = (Int, Int) -- horizontal position, depth

data Move = Move Direction Int

data Direction = Forward | Down | Up

parse :: String -> [Move]
parse = map parseMove . lines

parseMove :: String -> Move
parseMove str =
  let [d, n] = words str
  in Move (parseDirection d) (read n)

parseDirection :: String -> Direction
parseDirection = \case
  "forward" -> Forward
  "down" -> Down
  "up" -> Up
  d -> error $ "unable to parse direction: " ++ d

execute :: Pos -> [Move] -> Pos
execute = foldl executeOne

executeOne :: Pos -> Move -> Pos
executeOne (horiz, d) (Move dir n) = case dir of
  Forward -> (horiz + n, d)
  Down -> (horiz, d + n)
  Up -> (horiz, d - n)

data State = State
  { horizontal :: Int
  , depth :: Int
  , aim :: Int
  } deriving (Eq, Show)

initState :: State
initState = State 0 0 0

execute2 :: State -> [Move] -> State
execute2 = foldl executeOne2

executeOne2 :: State -> Move -> State
executeOne2 state (Move dir n) = case dir of
  Forward -> state
    { horizontal = horizontal state + n
    , depth = depth state + aim state * n
    }
  Down -> state { aim = aim state + n }
  Up -> state { aim = aim state - n }
