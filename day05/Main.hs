{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.State.Lazy (State, evalState, get, gets, modify)
import Data.Either (fromRight)
import qualified Data.Map as M
import Data.Text (pack)
import Text.Parsec.Text (Parser)
import Text.Parsec (
  (<|>), char, digit, letter, many, many1, optional, string, try, parse)


-- | Parsing
type Line = [[Char]]          -- intermediate representation of single line
type Stack  = [Char]          -- single crate stack
type Stacks = M.Map Int Stack
type Move = (Int, Int, Int)   -- num, from, to

parseFile :: Parser (Stacks, [Move])
parseFile = do stacks <- buildStacks <$> many1 stackLine
               _ <- many $ char ' ' <|> eol <|> digit
               moves <- many move
               return (stacks, moves)

move :: Parser Move
move = do num  <- string "move " >> int
          from <- string " from " >> int
          to   <- string " to " >> int <* optional eol
          return (num, from, to)
  where int = read <$> many1 digit

eol :: Parser Char
eol = char '\n'

stackLine :: Parser Line
stackLine = many (cell <|> emptyCell) <* eol
  where cell = char '[' >> many1 letter <* char ']' <* (optional $ char ' ')
        emptyCell = (try $ string "   " >> (optional $ char ' ')) >> return []

buildStacks :: [Line] -> Stacks
buildStacks xs = M.fromList $ zip [1..] (foldr1 (zipWith (++)) xs)

-- | Simulating crate moves
type Index     = Int
data MachineType = T9000 | T9001 deriving Eq

tops :: MachineType -> [Move] -> State Stacks String
tops _ [] = gets $ (map (head . snd)) . M.toAscList
tops b ((n, f, t):xs) = do s <- pop f n
                           push b t s
                           tops b xs

pop :: Index -> Int -> State Stacks Stack
pop i n = do st <- get
             let (popped, rest) = splitAt n $ M.findWithDefault [] i st
             modify $ M.insert i rest
             return popped

push :: MachineType -> Index -> Stack -> State Stacks ()
push t i s = modify $ M.insertWith merge i s
  where merge xs ys = if t == T9001 then xs ++ ys else reverse xs ++ ys

main :: IO ()
main = do
  input <- pack <$> readFile "data/day05.txt"
  let (stacks, moves) = fromRight (M.empty, []) $ parse parseFile "" input
  let topCrates = evalState (tops T9000 moves) stacks
  putStrLn $ "top crates (part  I): " ++ topCrates
  let topCrates' = evalState (tops T9001 moves) stacks
  putStrLn $ "top crates (part II): " ++ topCrates'
