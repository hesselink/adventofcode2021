{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Char (digitToInt)
import Data.Maybe (mapMaybe)
import Data.List (foldl1')

main :: IO ()
main = do
  f <- readFile "input/18"
  let ns = map parseNum . lines $ f
      finalSum = foldl1' add ns
      result = magnitude finalSum
  print result
  let magnitudes = [ magnitude (add x y) | x <- ns, y <- ns ]
      result2 = maximum magnitudes
  print result2

-- represent the tree as a list of numbers, increases and decreases in
-- levels

type SNum = [SElem]

data SElem = Reg Int | Inc | Dec
  deriving (Show, Eq)

data SNumZip = SNumZip
  { left :: SNum -- reversed
  , cur :: SElem
  , right :: SNum
  } deriving (Show, Eq)

add :: SNum -> SNum -> SNum
add x y =
  let res = reduce $ Inc : x ++ y ++ [Dec]
  in res

reduce :: SNum -> SNum
reduce = fromZip . go . toZip
  where
    toZip [] = error "toZip on empty list"
    toZip (hd:tl) = SNumZip [] hd tl
    fromZip (SNumZip l c r) = reverse l ++ [c] ++ r
    go z =
      let z' = explode (0 :: Int) z
          mz'' = split (reset z')
      in case mz'' of
        Just z'' -> go (reset z'')
        Nothing -> z'
    explode _ z@(SNumZip _ _ []) = z
    explode n (SNumZip l Inc (Reg x:Reg y:Dec:r)) | n > 3 = addL n x l (Reg 0:addR y r)
    explode d (SNumZip l Inc (r:rs)) = explode (d + 1) (SNumZip (Inc:l) r rs)
    explode d (SNumZip l Dec (r:rs)) = explode (d - 1) (SNumZip (Dec:l) r rs)
    explode d (SNumZip l (Reg n) (r:rs)) = explode d (SNumZip (Reg n:l) r rs)
    addR _ [] = []
    addR v (Inc:ns) = Inc:addR v ns
    addR v (Dec:ns) = Dec:addR v ns
    addR v ((Reg v'):ns) = (Reg (v+v')):ns
    addL n _ [] (r:rs) = explode n (SNumZip [] r rs)
    addL n v (Inc:l) r = addL (n-1) v l (Inc:r)
    addL n v (Dec:l) r = addL (n+1) v l (Dec:r)
    addL n v ((Reg v'):l) r = explode n (SNumZip l (Reg (v+v')) r)
    addL _ _ _ _ = error "addL"
    split (SNumZip l (Reg n) (r:rs)) | n > 9 = Just $ SNumZip l Inc (Reg (n `div` 2):Reg (n `div` 2 + n `mod` 2):Dec:r:rs)
    split (SNumZip l c (r:rs)) = split (SNumZip (c:l) r rs)
    split (SNumZip _ _ []) = Nothing
    reset (SNumZip l c r) =
      let (x:xs) = reverse l
      in SNumZip [] x (xs ++ c:r)


parseNum :: String -> SNum
parseNum = mapMaybe parseChar
  where
    parseChar = \case
      '[' -> Just Inc
      ']' -> Just Dec
      ',' -> Nothing
      d -> Just . Reg . digitToInt $ d

ppr:: SNum -> String
ppr [] = ""
ppr (Inc:xs) = '[':ppr xs
ppr (Dec:Inc:xs) = "],[" ++ ppr xs
ppr (Dec:Reg x:xs) = "]," ++ show x ++ ppr xs
ppr (Dec:xs) = ']':ppr xs
ppr (Reg x:Inc:xs) = show x ++ ",[" ++ ppr xs
ppr (Reg x:Reg y:xs) = show x ++ "," ++ show y ++ ppr xs
ppr (Reg x:xs) = show x ++ ppr xs

magnitude :: SNum -> Int
magnitude = mTree . toTree
  where
    mTree (Leaf n) = n
    mTree (Node l r) = 3 * mTree l + 2 * mTree r

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Eq)

toTree :: SNum -> Tree Int
toTree xs = fst $ go xs
  where
    go [] = error "toTree"
    go (Reg x:rs) = (Leaf x, rs)
    go (Inc:rs) =
      let (l, rs') = go rs
          (r, rs'') = go rs'
      in (Node l r, rs'')
    go (Dec:rs) = go rs
