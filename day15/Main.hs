module Main (main) where

import Data.Either (fromRight)
import qualified Data.List as L
import qualified Data.Set as S
import Data.Text (pack)
import Text.Parsec.Text (Parser)
import Text.Parsec ((<|>), char, digit, letter, many, many1, oneOf, option, parse)


type Vec2 = (Int, Int)

file :: Parser [(Vec2, Vec2)]
file = many $ (,) <$> point <*> point
  where point = (,) <$> (noDigit >> sint <* noDigit) <*> (sint <* noDigit)
        noDigit = many $ letter <|> (oneOf ":=, \n")
        sint = read <$> ((:) <$> (option ' ' (char '-')) <*> (many1 digit))

man :: Vec2 -> Vec2 -> Int
man (x, y) (x', y') = (abs $ x - x') + (abs $ y - y')

type Ball = (Vec2, Int)

getInters :: Int -> Ball -> S.Set Vec2
getInters row (c@(cx, _), r) = S.fromList [(cx+s,row) | s <- [(-offs)..(offs)]]
  where offs = r - d
        d = man c (cx, row)

freq :: Vec2 -> Int
freq (x,y) = x * 4000000 + y

covInterval :: Int -> Ball -> [(Int,Int)]
covInterval row (c@(cx,_),r) = if offs <= 0 then [] else [(cx-offs, cx+offs)]
  where offs = r - d
        d = man c (cx, row)

covIntervals :: Int -> [Ball] -> [(Int, Int)]
covIntervals row bs = L.sort $ bs >>= (covInterval row)

findUncovered :: Int -> [(Int, Int)] -> Int -> [Int]
findUncovered m [] curMax = [(curMax+1)..m]
findUncovered m ((a,b):xs) curMax = [(curMax+1)..(a-1)] ++ findUncovered m xs newMax
  where newMax = max b curMax

getUncR :: Int -> [Ball] -> Int -> [Vec2]
getUncR m bs row = map (\x -> (x,row)) (findUncovered m (covIntervals row bs) 0)

getUnc :: Int -> [Ball] -> [Vec2]
getUnc m bs  = [0..m] >>= getUncR m bs

main :: IO ()
main = do
  input <- pack <$> readFile "data/day15.txt"
  let xs = fromRight [] $ parse file "" input
  let bs = map (\(s, b) -> (s, man s b)) xs

  let cov = foldl (\s b -> getInters 2000000 b `S.union` s) S.empty bs
  let beacons = map snd xs
  let cov' = foldl (\s b -> S.delete b s) cov beacons
  putStrLn $ "part I: " ++ show (S.size cov')

  let mm = 4000000
  let rr = getUnc mm bs
  let fs = map freq rr
  putStrLn $ "part II: " ++ show fs
