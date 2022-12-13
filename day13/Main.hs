module Main (main) where

import Data.Either (fromRight)
import qualified Data.List as L
import Data.Text (pack)
import Text.Parsec.Text (Parser)
import Text.Parsec ((<|>), char, digit, many1, sepBy, try, parse)

-- | Ordering
data Packet = N Int | Packet [Packet] deriving (Eq, Show)

instance Ord Packet where
  compare (Packet xs) (Packet ys) = compare xs ys
  compare l@(N _) r@(Packet _)    = compare (Packet [l]) r
  compare l@(Packet _) r@(N _)    = compare l (Packet [r])
  compare (N l) (N r)             = compare l r

-- | Parsing
file :: Parser [(Packet, Packet)]
file = pair `sepBy` (try $ eol >> eol)
  where pair = (,) <$> packet <* eol <*> packet
        eol = char '\n'

packet :: Parser Packet
packet = (Packet <$> list) <|> num
  where list = char '[' >> packet `sepBy` comma <* char ']'
        num = N . read <$> many1 digit
        comma = char ','

-- Get 0-based indices of right orderings
ltIndices :: [Ordering] -> [Int]
ltIndices = L.findIndices (==LT)

main :: IO ()
main = do
  -- part I
  input <- pack <$> readFile "data/day13.txt"
  let pairs = fromRight [] $ parse file "" input
  let indSum = sum . (map (+1)) . ltIndices. (map (uncurry compare)) $ pairs
  putStrLn $ "Sum of indices (part I): " ++ show indSum
  -- part II
  let ds = [Packet [Packet [N n]] | n <- [2, 6]]
  let ls = L.sort $ ds ++ (pairs >>= (\(x, y) -> [x, y]))
  let p = product . (map (+1)) . (L.findIndices (`L.elem` ds)) $ ls
  putStrLn $ "Product of divider indices (part II): " ++ show p
