module Main where

import Data.List (sort)

main :: IO ()
main = do
  f <- readFile "input/10"
  let ls = lines f
      result = sum . map score $ ls
  print result
  let result2 = calculateScore . filter (/= 0) . map score2 $ ls
  print result2

score :: String -> Int
score = either errorToScore (const 0) . parse

data BraceType
  = Paren
  | Square
  | Curly
  | Triangle
  deriving (Show, Eq)

data Error
  = Mismatch BraceType
  | Incomplete [BraceType]

errorToScore :: Error -> Int
errorToScore (Incomplete _) = 0 -- Ignore incomplete in part 1
errorToScore (Mismatch bt) =
  case bt of
    Paren -> 3
    Square -> 57
    Curly -> 1197
    Triangle -> 25137

parse :: String -> Either Error ()
parse = go []
  where
    go []      [] = Right ()
    go stack   [] = Left (Incomplete stack)
    go (s:st) (p:ps) | isClose p = if braceType p /= s
                                    then Left (Mismatch (braceType p))
                                    else go st ps
    go []     (p:_)  | isClose p = Left (Mismatch (braceType p))
    go stack  (p:ps) = go (braceType p : stack) ps

isClose :: Char -> Bool
isClose = (`elem` ")]}>")

braceType :: Char -> BraceType
braceType c =
  if c `elem` "()" then Paren
  else if c `elem` "[]" then Square
  else if c `elem` "{}" then Curly
  else if c `elem` "<>" then Triangle
  else error $ "Unknown brace type: " ++ [c]

score2 :: String -> Int
score2 = either errorToScore2 (const 0) . parse

errorToScore2 :: Error -> Int
errorToScore2 (Incomplete st) = foldl step 0 st
  where
   step sc c = 5 * sc + case c of
     Paren -> 1
     Square -> 2
     Curly -> 3
     Triangle -> 4
errorToScore2 (Mismatch _) = 0 -- Ignore mismatch in part 2

calculateScore :: [Int] -> Int
calculateScore ss = head . drop (length ss `div` 2) . sort $ ss
