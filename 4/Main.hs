module Main where

import Data.List.Split (splitOn)
import Data.List (intercalate, findIndex, transpose)
import Data.Maybe (isNothing, fromMaybe)

main :: IO ()
main = do
  f <- readFile "input/4"
  let startState = parse f
      endState = runUntilFirstBingo startState
      [winningBoard] = filter hasBingo (boards endState)
      result = score (lastDraw endState) winningBoard
  print result
  let endState2 = runUntilBeforeFinalBingo startState
      (d, b) = lastBoardState endState2
      result2 = score d b
  print result2

data State = State
  { draws :: [Int]
  , lastDraw :: Int
  , boards :: [Board]
  } deriving (Show, Eq)

type Board = [[Maybe Int]]

parse :: String -> State
parse str =
  let (hd:tl) = lines str
      ds = map read . splitOn "," $ hd
      bs = map parseBoard . filter (/= []) . splitOn [""] $ tl
  in State ds 0 bs

parseBoard :: [String] -> Board
parseBoard = map (map (Just . read) . words)

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
crossOffOne n = map (map (\v -> if v == Just n then Nothing else v))

anyHasBingo :: State -> Bool
anyHasBingo = any hasBingo . boards

hasBingo :: Board -> Bool
hasBingo b =
  let b2 = transpose b
  in any (all isNothing) b || any (all isNothing) b2

ppr :: State -> String
ppr st = intercalate "\n\n" $
  intercalate "," (map show (draws st))
  :
  map pprBoard (boards st)

pprBoard :: Board -> String
pprBoard = intercalate "\n"
         . map (intercalate " " . map (maybe "." showNum))

showNum :: Int -> String
showNum n | n < 10 = " " <> show n
          | otherwise = show n

score :: Int -> Board -> Int
score d = (* d) . sum . map (fromMaybe 0) . concat

runUntilBeforeFinalBingo :: State -> State
runUntilBeforeFinalBingo st =
  let st' = drawOne st
  in if allHaveBingo st'
     then st
     else runUntilBeforeFinalBingo st'

allHaveBingo :: State -> Bool
allHaveBingo = all hasBingo . boards

lastBoardState :: State -> (Int, Board)
lastBoardState st =
  let st' = drawOne st
      Just ix = findIndex (not . hasBingo) (boards st)
  in (lastDraw st', boards st' !! ix)
