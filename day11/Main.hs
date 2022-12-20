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
                           , _worryMod      :: Int -> Int
                           , _maxLvl        :: Int
                           }
makeLenses ''IterState

initState :: Bool -> [(Monkey, [Int], Int)] -> IterState
initState partII xs =
  IterState { _roundsLeft    = if partII then 10000 else 20
            , _inspectCounts = M.fromList $ zip [0..] $ replicate n 0
            , _items         = M.fromList $ zip [0..] zs
            , _monkeys       = M.fromList $ zip [0..] ys
            , _currentMonkey = 0
            , _numMonkeys    = n
            , _worryMod      = if partII then id else flip div 3
            , _maxLvl        = product vs
            }
  where (ys, zs, vs) = unzip3 xs
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
playMonkey c = do xs <- M.findWithDefault [] c <$> use items
                  inspectCounts %= M.insertWith (+) c (length xs)
                  m  <- fromJust .  M.lookup c <$> use monkeys
                  f <- use worryMod
                  ml <- use maxLvl
                  let ys = map ((`mod` ml) . f . (op m)) xs
                  let ds = map (to m) ys
                  (uncurry sendTo) `mapM_` zip ys ds
                  items %= M.insert c []

sendTo :: Int -> Int -> State IterState ()
sendTo i m = items %= M.insertWith (flip (++)) m [i]

monkey :: Parser (Monkey, [Int], Int)
monkey = do _ <- noDigit >> int >> noDigit >> spaces >> noDigit
            xs <- int `sepBy` (string ", ") <* spaces
            f <- noDigit >> operation
            x <- noDigit >> int <* spaces
            a <- noDigit >> int <* spaces
            b <- noDigit >> int <* spaces
            return $ (Monkey f (\n -> if n `mod` x == 0 then a else b), xs, x)

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

getTwoMaxProd :: IterState -> Int
getTwoMaxProd =  product . (take 2) . sortDesc . M.elems . _inspectCounts
  where sortDesc = L.sortBy (flip compare)

main :: IO ()
main = do
  input <- pack <$> readFile "data/day11.txt"
  let ms = fromRight [] $ parse (many monkey) "" input
  let fs = execState play $ initState False ms
  putStrLn $ show $ getTwoMaxProd fs

  let fs' = execState play $ initState True ms
  putStrLn $ show $ getTwoMaxProd fs'
