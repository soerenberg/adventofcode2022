{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad.State.Lazy (State, execState)
import Data.Either (fromRight)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Text (pack)
import Lens.Micro.Platform ((%=), (.=), makeLenses, use)
import Text.Parsec.Text (Parser)
import Text.Parsec ((<|>), digit, letter, many, many1, oneOf, sepBy, string, try, parse)


data Monkey = Monkey { op :: Int -> Int
                     , to :: Int -> Int
                     }

data IterState = IterState { _roundsLeft    :: Int
                           , _inspectCounts :: M.Map Int Int
                           , _items         :: M.Map Int [Int]
                           , _monkeys       :: M.Map Int Monkey
                           , _currentMonkey :: Int
                           , _numMonkeys    :: Int
                           }
makeLenses ''IterState

initState :: [(Monkey, [Int])] -> IterState
initState xs = IterState { _roundsLeft    = 20
                         , _inspectCounts = M.fromList $ zip [0..] $ replicate n 0
                         , _items         = M.fromList $ zip [0..] zs
                         , _monkeys       = M.fromList $ zip [0..] ys
                         , _currentMonkey = 0
                         , _numMonkeys    = n
                         }
  where (ys, zs) = unzip xs
        n = length ys

play :: State IterState ()
play = do isDone <- (<=0) <$> use roundsLeft
          if isDone
            then return ()
            else do roundsLeft %= (subtract 1)
                    n <- use numMonkeys
                    playMonkey `mapM_` [0..(n-1)]
                    play

playMonkey :: Int ->  State IterState ()
playMonkey c = do --c <- use currentMonkey
                  xs <- M.findWithDefault [] c <$> use items
                  inspectCounts %= M.insertWith (+) c (length xs)
                  m  <- fromJust .  M.lookup c <$> use monkeys
                  let ys = map ((flip div 3) . (op m)) xs
                  let ds = map (to m) ys
                  (uncurry sendTo) `mapM_` zip ys ds
                  items %= M.insert c []

sendTo :: Int -> Int -> State IterState ()
sendTo i m = items %= M.insertWith (flip (++)) m [i]

monkey :: Parser (Monkey, [Int])
monkey = do _ <- noDigit >> int >> noDigit >> spaces >> noDigit
            xs <- int `sepBy` (string ", ") <* spaces
            f <- noDigit >> operation
            x <- noDigit >> int <* spaces
            a <- noDigit >> int <* spaces
            b <- noDigit >> int <* spaces
            return $ (Monkey f (\n -> if n `mod` x == 0 then a else b), xs)

spaces :: Parser String
spaces = many $ oneOf " \n"

noDigit :: Parser String
noDigit = many (letter <|> oneOf "=: ")

operation :: Parser (Int -> Int)
operation = square <|> mult <|> add
  where square = try $ string "* old" >> return (^2) <* spaces
        mult = string "* " >> (\n -> (\x -> x*n)) <$> int <* spaces
        add = string "+ " >> (\n -> (\x -> x+n)) <$> int <* spaces

int :: Parser Int
int = read <$> many1 digit


main :: IO ()
main = do
  input <- pack <$> readFile "data/day11.txt"
  let ms = fromRight [] $ parse (many monkey) "" input
  let st = initState ms
  let fs = execState play st
  let r = product . (take 2) . (L.sortBy (flip compare)) . M.elems . _inspectCounts $ fs
  putStrLn $ (show r)

