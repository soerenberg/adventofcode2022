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
neighbors (x, y, z) = [ (x-1, y, z), (x+1, y, z), (x, y-1, z), (x, y+1, z)
                      , (x, y, z-1), (x, y, z+1)]

boundingBoxSize :: [Vec3] -> Int
boundingBoxSize vs = maximum $ vs >>= (\(a,b,c) -> [a,b,c])

data IterState = IterState { _vecs          :: S.Set Vec3
                           , _exposedFacets :: S.Set (Vec3, Vec3)
                           , _gridSize      :: Int
                           -- , _gridLength    :: Int
                           , _visited       :: S.Set Vec3
                           -- , _pos           :: Vec3
                           } deriving Show
makeLenses ''IterState

initState :: Int -> [Vec3] -> IterState
initState m vs = IterState { _vecs          = S.fromList vs
                           , _exposedFacets = S.empty
                           , _gridSize      = m
                           , _visited       = S.empty
                           -- , _pos
                           }


countExposedFacets :: Vec3 -> State IterState ()
countExposedFacets pos = do visited %= S.insert pos
                            m <- use gridSize
                            let neighs = neighborsBounds m pos
                            (visit pos) `mapM_` neighs

                            -- (S.size) <$> use exposedFacets

neighborsBounds :: Int -> Vec3 -> [Vec3]
neighborsBounds m v = filter cond $ neighbors v
  where cond (x, y, z) = inBound x && inBound y && inBound z
        inBound x = -1 <= x && x <= m+2

visit :: Vec3 -> Vec3 -> State IterState ()
visit v w = do isFace <- (w `S.member`) <$> use vecs
               isVisited <- (w `S.member`) <$> use visited
               if isFace
                 then exposedFacets %= S.insert (orderVecs v w)
                 else if isVisited
                        then return ()
                        else countExposedFacets w


main :: IO ()
main = do
  input <- pack <$> readFile "data/day18.txt"
  let vs = fromRight [] $ parse file "" input
  let (ws, es) = foldr insert (S.empty, S.empty) vs
  let n = 6 * (S.size ws) - 2 * (S.size es)  -- count degrees
  putStrLn $ "part I: " ++ show n

  -- part II
  let m = boundingBoxSize vs
  let s = execState (countExposedFacets (1,1,1)) (initState m vs)
  putStrLn $ "part II: " ++ (show . S.size $ _exposedFacets s)
