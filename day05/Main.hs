{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Either (fromRight)
import qualified Data.Map as M
import Data.Text (pack)
import Text.Parsec.Text (Parser)
import Text.Parsec (
  (<|>), char, digit, letter, many, many1, optional, string, try, parse)


type Line = [[Char]]          -- intermediate representation of single line
type Stack  = [Char]          -- single crate stack
type Stacks = M.Map Int Stack
type Move = (Int, Int, Int)   -- num, from, to

eol :: Parser Char
eol = char '\n'

stackLine :: Parser Line
stackLine = many (cell <|> emptyCell) <* eol
  where cell = char '[' >> many1 letter <* char ']' <* (optional $ char ' ')
        emptyCell = (try $ string "   " >> (optional $ char ' ')) >> return []

int :: Parser Int
int = read <$> many1 digit

move :: Parser Move
move = do num  <- string "move " >> int
          from <- string " from " >> int
          to   <- string " to " >> int <* optional eol
          return (num, from, to)

buildStacks :: [Line] -> Stacks
buildStacks xs = M.fromList $ zip [1..] (foldr1 (zipWith (++)) xs)

parseFile :: Parser (Stacks, [Move])
parseFile = do stacks <- buildStacks <$> many1 stackLine
               _ <- many $ char ' ' <|> eol <|> digit
               moves <- many move
               return (stacks, moves)

type Index     = Int
type NumCrates = Int

simMove :: Stacks -> Move -> Stacks
simMove s (n, f, t) = uncurry (putAt t) (popFrom f n s)

simMoves :: Stacks -> [Move] -> Stacks
simMoves s ms = foldl simMove s ms

popFrom :: Index -> NumCrates -> Stacks -> (Stack, Stacks)
popFrom i num s = (popped, M.insert i rest s)
  where (popped, rest) = splitAt num $ M.findWithDefault [] i s

putAt :: Index -> Stack -> Stacks -> Stacks
putAt i rest s = M.insertWith (\a b -> reverse a ++ b) i rest s

getTops :: Stacks -> String
getTops xs = map (head' . snd) $ M.toAscList xs
  where head' [] = ' '
        head' ys = head ys

main :: IO ()
main = do
  input <- pack <$> readFile "data/day05.txt"
  let (stacks, moves) = fromRight (M.empty, []) $ parse parseFile "" input
  let topCrates = getTops $ simMoves stacks moves
  putStrLn $ "top crates: " ++ topCrates
