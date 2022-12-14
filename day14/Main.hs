module Main (main) where

import Data.Either (fromRight)
import qualified Data.Map as M
import Data.Text (pack)
import Text.Parsec.Text (Parser)
import Text.Parsec (char, digit, many, many1, oneOf, sepBy, string, parse)


type Vec2 = (Int, Int)
data Cell = Air | Rock deriving (Eq, Show)
type Cave = M.Map Vec2 Cell

cave :: Parser Cave
cave = do ls <- many line
          return $ foldr insertLines M.empty ls

line :: Parser [Vec2]
line = vec `sepBy` (string " -> ") <* spaces

spaces :: Parser String
spaces = many1 $ oneOf " \n"

vec :: Parser Vec2
vec = (,) <$> num <* char ',' <*> num
  where num = read <$> many1 digit

insertLines :: [Vec2] -> Cave -> Cave
insertLines (x:y:xs) c = foldl (\c'' a -> M.insert a Rock c'') c' (conn x y)
  where c' = insertLines (y:xs) c
insertLines _ c = c

bottomLine :: Cave -> Int
bottomLine = maximum . (map snd) . M.keys

conn :: Vec2 -> Vec2 -> [Vec2]
conn l@(x,y) r@(a,b)
  | a < x || b < y = conn r l
  | otherwise      = [(i,j) | i <- [x..a], j <- [y..b]]

type SimStep = Int -> Vec2 -> Cave -> (Cave, Bool)

simI :: SimStep
simI m (x,y) c
  | y > m = (c, True)
  | M.findWithDefault Air (x,y+1) c == Air = simI m (x,y+1) c
  | M.findWithDefault Air (x-1,y+1) c == Air = simI m (x-1,y+1) c
  | M.findWithDefault Air (x+1,y+1) c == Air = simI m (x+1,y+1) c
  | otherwise = (M.insert (x,y) Rock c, False)

simII :: SimStep
simII m (x,y) c
  | (x,y) == (500,0) && M.findWithDefault Air (x,y) c == Rock = (c, True)
  | (y<=m) && M.findWithDefault Air (x,y+1) c == Air = simII m (x,y+1) c
  | (y<=m) && M.findWithDefault Air (x-1,y+1) c == Air = simII m (x-1,y+1) c
  | (y<=m) && M.findWithDefault Air (x+1,y+1) c == Air = simII m (x+1,y+1) c
  | otherwise = (M.insert (x,y) Rock c, False)

runSimulation :: SimStep -> Int -> Int -> Cave -> Int
runSimulation f n cnt c = case f n (500, 0) c of
                (_, True) -> cnt
                (c', _) -> runSimulation f n (cnt+1) c'

main :: IO ()
main = do
  -- part I
  input <- pack <$> readFile "data/day14.txt"
  let c = fromRight M.empty $ parse cave "" input
  putStrLn $ "part  I: " ++ (show $ runSimulation simI (bottomLine c) 0 c)
  putStrLn $ "part II: " ++ (show $ runSimulation simII (bottomLine c) 0 c)
