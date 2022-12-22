{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad.State (State, evalState, execState)
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Text (pack)
import Data.Tuple (swap)
import Lens.Micro.Platform ((%=), (.=), makeLenses, use, zoom)
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>), char, digit, letter, many, many1, oneOf, parse)

data Move = F Int | L | R deriving (Eq, Show)

parseMoves :: Parser [Move]
parseMoves = many $ int <|> rot
  where int = (F . read) <$> many1 digit
        rot = (char 'L' >> return L) <|> (char 'R' >> return R)

type Vec2 = (Int, Int)
type Cave = M.Map Vec2 Bool

buildCave :: [String] -> Cave
buildCave xs = M.fromList $ concat $ map (row 1) $ zip [1..] xs

row :: Int -> (Int, String) -> [(Vec2, Bool)]
row _ (_, []) = []
row i (j, (x:xs))
  | x == '#'  = ((i,j), True):(row (i+1) (j,xs))
  | x == '.'  = ((i,j), False):(row (i+1) (j,xs))
  | otherwise = row (i+1) (j,xs)

data Dir = E | S | W | N deriving (Enum, Eq, Show)

data IterState = IterState { _pos    :: Vec2
                           , _moves  :: [Move]
                           , _dir    :: Dir
                           , _cave   :: Cave
                           , _partII :: Bool
                           } deriving Show
makeLenses ''IterState

initState :: Cave -> [Move] -> Bool -> IterState
initState c ms b= IterState { _pos   = swap . minimum . (map swap) . M.keys $ c
                           , _moves  = ms
                           , _dir    = E
                           , _cave   = c
                           , _partII = b
                           }

play :: State IterState Int
play = do isDone <- null <$> use moves
          if isDone
            then do (x, y) <- use pos
                    d <- (fromEnum) <$> use dir
                    return $ 4*x + 1000*y + d
            else do m <- head <$> use moves
                    moves %= tail
                    case m of
                      (F n) -> go n
                      L -> dir %= toEnum . (`mod` 4) . (subtract 1) . fromEnum
                      R -> dir %= toEnum . (`mod` 4) . (+1) . fromEnum
                    play

go :: Int -> State IterState ()
go 0 = return ()
go n = do d <- use dir
          p <- use pos
          c <- use cave
          p2 <- use partII
          let (nextp, nextd) = if p2 then nextPos3d d p else (nextPos c d p, d)
          if isBlocked nextp c
            then return ()
            else do pos .= nextp
                    dir .= nextd
                    go (n-1)

ahead :: Dir -> Vec2 -> Vec2
ahead N (x, y) = (x, y - 1)
ahead S (x, y) = (x, y + 1)
ahead W (x, y) = (x - 1, y)
ahead E (x, y) = (x + 1, y)

nextPos :: Cave -> Dir -> Vec2 -> Vec2
nextPos c d p = if inbound then nextp else goUntilWall c (reverseDir d) p
  where nextp = ahead d p
        inbound = M.member nextp c
        reverseDir = toEnum . (`mod` 4) . (+2) . fromEnum

goUntilWall :: Cave -> Dir -> Vec2 -> Vec2
goUntilWall c d p = if next `M.member` c
                      then goUntilWall c d next
                      else p
  where next = ahead d p

nextPos3d :: Dir -> Vec2 -> (Vec2, Dir)
nextPos3d d p@(x, y)
  | d == W && (x == 1)  && (y > 100)  && (y <= 150) = ((51, 151 - y), E)
  | d == W && (x == 51) && (y > 0)   && (y <= 50)  = ((1, 151 - y), E)
  | d == W && (x == 1) && (y > 150) && (y <= 200) = ((y-100, 1), S)
  | d == N && (y == 1) && (x > 50) && (x <= 100) = ((1, x+100), E)
  | d == N && (x > 0) && (x <= 50) && (y == 101) = ((51, 50+x), E)
  | d == W && (x == 51) && (y > 50) && (y <= 100) = ((y-50, 101), S)
  | d == E && (x == 100) && (y > 50) && (y <= 100) = ((y+50, 50), N)
  | d == S && (y == 50) && (x > 100) && (x <= 150) = ((100, x-50), W)
  | d == E && (x == 100) && (y > 100) && (y <= 150) = ((150,151-y), W)
  | d == E && (x == 150) && (y > 0)   && (y <= 50)  = ((100,151-y), W)
  | d == E && (x == 50) && (y > 150) && (y <= 200) = ((y-100,150), N)
  | d == S && (y == 150) && (x > 50)   && (x <= 100)  = ((50,x+100), W)
  | d == S && (y == 200) && (x > 0) && (x <= 50) = ((x+100, 1), S)
  | d == N && (y == 1) && (x > 100)   && (x <= 150)  = ((x-100, 200), N)
  | otherwise = (ahead d p, d)

isBlocked :: Vec2 -> Cave -> Bool
isBlocked = M.findWithDefault False

main :: IO ()
main = do
  input <- readFile "data/day22.txt"

  let ms = (fromRight []) . (parse parseMoves "") . last . lines $ input
  let c = buildCave . init . lines $ input

  let r = evalState play (initState c ms False)
  putStrLn $ "part I (149250): " ++ (show $ r)

  let r' = evalState play (initState c ms True)
  putStrLn $ "part II (12462): " ++ show  r'
