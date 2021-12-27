{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Char (digitToInt, intToDigit)
import Numeric (showIntAtBase)
import Control.Monad (replicateM)
import Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

import Lib

main :: IO ()
main = do
  f <- readFile "input/16"
  let bs = toBits (init f)
      pt = runParser parsePacket bs
      result = sumVersions pt
  print result
  let result2 = value pt
  print result2

type Bit = Char

toBits :: String -> [Bit]
toBits = concatMap hexToBits
  where
    hexToBits = intToBin . digitToInt
    intToBin i = padWith '0' 4 $ showIntAtBase 2 intToDigit i ""

padWith :: a -> Int -> [a] -> [a]
padWith c l s | length s >= l = s
              | otherwise = replicate (l - length s) c ++ s

sumVersions :: Packet -> Int
sumVersions (Packet v pt) = v + case pt of
  Literal _ -> 0
  Operator _ ps -> sum (map sumVersions ps)

value :: Packet -> Int
value (Packet _ pt) = case pt of
  Literal n -> n
  Operator o ps ->
    case o of
      Sum -> sum (map value ps)
      Product -> product (map value ps)
      Minimum -> minimum (map value ps)
      Maximum -> maximum (map value ps)
      GreaterThan ->
        let [p1, p2] = ps
        in if value p1 > value p2 then 1 else 0
      LessThan ->
        let [p1, p2] = ps
        in if value p1 < value p2 then 1 else 0
      EqualTo ->
        let [p1, p2] = ps
        in if value p1 == value p2 then 1 else 0

data Packet = Packet
  { version :: Int
  , content :: PacketType
  } deriving (Show, Eq)

data PacketType
  = Literal Int
  | Operator Op [Packet]
  deriving (Show, Eq)

data Op = Sum | Product | Minimum | Maximum | GreaterThan | LessThan | EqualTo
  deriving (Show, Eq)

type Parser = P.Parsec Void String

runParser :: Parser a -> String -> a
runParser p = either (error . P.errorBundlePretty) id . P.runParser (p <* dropZeroes) "input"

dropZeroes :: Parser ()
dropZeroes = () <$ P.many (P.char '0')

parsePacket :: Parser Packet
parsePacket = do
  v <- parseVersion
  tid <- parseTypeID
  case tid of
    4 -> (Packet v . Literal) <$> parseLiteral
    o -> (Packet v . Operator (parseOp o)) <$> parseOperator

parseOp :: Int -> Op
parseOp = \case
  0 -> Sum
  1 -> Product
  2 -> Minimum
  3 -> Maximum
  5 -> GreaterThan
  6 -> LessThan
  7 -> EqualTo
  n -> error $ "Unknown operator: " ++ show n

parseOperator :: Parser [Packet]
parseOperator = do
  lType <- parseLengthTypeID
  case lType of
    TotalLength l -> do
      o <- P.getOffset
      parseUntilOffset (o + l) parsePacket
    NumPackets p -> P.count p parsePacket

parseUntilOffset :: Int -> Parser a -> Parser [a]
parseUntilOffset o p = do
  o' <- P.getOffset
  if o' < o
  then (:) <$> p <*> parseUntilOffset o p
  else pure []

data LengthTypeID = TotalLength Int | NumPackets Int
  deriving (Show, Eq)

parseLengthTypeID :: Parser LengthTypeID
parseLengthTypeID = do
  c <- P.satisfy (const True)
  case c of
    '0' -> (TotalLength . readBinUnsafe) <$> replicateM 15 (P.satisfy (const True))
    '1' -> (NumPackets . readBinUnsafe) <$> replicateM 11 (P.satisfy (const True))
    _ -> error $ "Not a bit: " ++ [c]

parseLiteral :: Parser Int
parseLiteral = f <$> P.many oneBlock <*> zeroBlock
  where
    oneBlock = do
      _ <- P.char '1'
      replicateM 4 (P.satisfy (const True))
    zeroBlock = do
      _ <- P.char '0'
      replicateM 4 (P.satisfy (const True))
    f :: [String] -> String -> Int
    f ss s = readBinUnsafe (concat (ss ++ [s]))

parseVersion :: Parser Int
parseVersion = parseThreeBitsAsNumber

parseTypeID :: Parser Int
parseTypeID = parseThreeBitsAsNumber

parseThreeBitsAsNumber :: Parser Int
parseThreeBitsAsNumber = fst . head . readBin <$> replicateM 3 (P.satisfy (const True))

-- parseBinary :: Pars
