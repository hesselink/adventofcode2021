module Main where

main :: IO ()
main = do
  f <- readFile "input/10"
  let ls = lines f
      result = sum . map score $ ls
  print result

score :: String -> Int
score = either errorToScore (const 0) . parse

data BraceType
  = Paren
  | Square
  | Curly
  | Triangle
  deriving (Show, Eq)

data Error = Mismatch BraceType

errorToScore :: Error -> Int
errorToScore (Mismatch bt) =
  case bt of
    Paren -> 3
    Square -> 57
    Curly -> 1197
    Triangle -> 25137

parse :: String -> Either Error ()
parse = go []
  where
    go _      [] = Right () -- ignore incomplete for now
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
