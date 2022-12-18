{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad.State.Lazy (State, execState)
import Data.Either (fromRight)
import qualified Data.Set as S
import Data.Text (pack)
import Lens.Micro.Platform ((%=), makeLenses, use)
import Text.Parsec.Text (Parser)
import Text.Parsec (digit, many, many1, oneOf, parse)


-- | Build graph representation
type Vec3 = (Int, Int, Int)
type Graph = (S.Set (Vec3), S.Set (Vec3, Vec3))

file :: Parser [Vec3]
file = many ((,,) <$> int <*> int <*> int)
  where int = read <$> many1 digit <* (oneOf ",\n")

insert :: Vec3 -> Graph -> Graph
insert v (vs, es) = insertNode v $ foldr insertEdge (vs, es) edges
  where edges = map (\w -> orderVecs v w) neighs
        neighs = filter (\w -> w `S.member` vs) $ neighbors v

orderVecs :: Vec3 -> Vec3 -> (Vec3, Vec3)
orderVecs a b = (min a b, max a b)

insertEdge :: (Vec3, Vec3) -> Graph -> Graph
insertEdge e (vs, es) = (vs, S.insert e es)

insertNode :: Vec3 -> Graph -> Graph
insertNode v (vs, es) = (S.insert v vs, es)

neighbors :: Vec3 -> [Vec3]
neighbors (x, y, z) = [(x-1,y,z), (x+1,y,z), (x,y-1,z), (x,y+1,z), (x,y,z-1), (x,y,z+1)]


main :: IO ()
main = do
  input <- pack <$> readFile "data/day18.txt"
  let vs = fromRight [] $ parse file "" input
  let (ws, es) = foldr insert (S.empty, S.empty) vs
  let n = 6 * (S.size ws) - 2 * (S.size es)  -- count degrees
  putStrLn $ "part I: " ++ show n
