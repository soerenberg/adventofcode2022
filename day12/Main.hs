{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.State.Lazy (State, execState)
import Data.Char (ord)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Set as S
import Lens.Micro.Platform ((%=), (.=), makeLenses , use)


-- | Build graph representation
type Coord = (Int, Int)
type Graph = M.Map Coord [Coord]

toCharMap :: String -> M.Map Coord Char
toCharMap s = M.fromList xs
  where xs = concat $ zipWith inner [1..] $ lines s
        inner n = zipWith (\i e -> ((n, i), e)) [1..]

toGraph :: (M.Map Coord Int) -> Graph
toGraph m = M.mapWithKey (\k _ -> nodeAdjList k m) m

findCharPos :: Char -> (M.Map (Int, Int) Char) -> Coord
findCharPos c = fst . head . (filter $ (==c) . snd) . M.toList

nodeAdjList :: Coord -> (M.Map Coord Int) -> [Coord]
nodeAdjList p@(x, y) m = [c | (c, e) <- neighElev, e - elev <= 1]
  where neighElev = catMaybes [(\q -> (c, q)) <$> M.lookup c m | c <- neighs]
        neighs = [(x + dx, y + dy) | (dx, dy) <- ds]
        ds = [(-1, 0), (1, 0), (0, -1), (0, 1)]
        elev = fromJust $ M.lookup p m

-- | Extended integers
data Length = N Int | Inf deriving (Eq, Show)
instance Ord Length where
  compare Inf Inf = EQ
  compare _ Inf = LT
  compare Inf _ = GT
  compare (N a) (N b) = compare a b

-- | State monad for Dijkstra's algorithm
data DState = DS { _graph         :: Graph
                 , _elevations    :: M.Map Coord Int
                 , _start         :: Coord
                 , _end           :: Coord
                 , _lengths       :: M.Map Coord Length
                 , _unvisited     :: S.Set Coord
                 , _current       :: Coord
                 , _currentWeight :: Length
                 } deriving (Eq, Show)
makeLenses ''DState

initState :: Graph -> Coord -> Coord -> Elevations -> DState
initState g s e es = DS { _graph         = g
                        , _elevations    = es
                        , _start         = s
                        , _end           = e
                        , _lengths       = ls
                        , _unvisited     = S.fromList $ M.keys g
                        , _current       = s
                        , _currentWeight = Inf
                        }
  where ls = M.insert s (N 0) $ M.map (const Inf) g

buildState :: Char -> String -> DState
buildState t xs = initState g s e es
  where g = toGraph es
        es = M.map toElevation m
        s = findCharPos t m
        e = findCharPos 'E' m
        m =  toCharMap xs

type Elevations = M.Map Coord Int

toElevation :: Char -> Int
toElevation 'S' = toElevation 'a'
toElevation 'E' = toElevation 'z'
toElevation x = ord x - 96

-- | Dijkstra's algorithm
dijkstra :: State DState ()
dijkstra = do isDone <- S.null <$> use unvisited
              case isDone of
                False -> do dijkstraStep
                            dijkstra
                _ -> return ()

dijkstraStep ::  State DState ()
dijkstraStep =
  do ls <- use lengths
     unv <- use unvisited
     uncurry updateCurrents $ findNext unv ls
     updateNeighbors
     return ()

updateCurrents :: Coord -> Length -> State DState ()
updateCurrents c w = do current .= c
                        currentWeight .= w
                        unvisited %= S.delete c

findNext :: (S.Set Coord) -> (M.Map Coord Length) -> (Coord, Length)
findNext s m = L.minimumBy cmp $ M.toList $ M.filterWithKey cond m
  where cmp x y = compare (snd x) (snd y)
        cond = (\x _ -> S.member x s)

updateNeighbors :: State DState ()
updateNeighbors =
  do c <- use current
     g <- use graph
     let cands = M.findWithDefault [] c g
     mapM_ updateNeighbor cands

updateNeighbor :: Coord -> State DState ()
updateNeighbor neigh =
  do w <- use currentWeight
     lengths %= M.insertWith min neigh (inc w)
     where inc Inf = Inf
           inc (N n) = N $ n + 1

-- | Extract length to 'E'
getLengthToEnd :: DState -> Length
getLengthToEnd s = ls
  where e = _end s
        ls = M.findWithDefault Inf e $ _lengths s

-- | Extract length to closest 'a'
shortestLengthToA :: DState -> Length
shortestLengthToA s = minimum ls
  where ls = map (flip (M.findWithDefault Inf) $ _lengths s) as
        as = M.keys $ M.filter (==1) $ _elevations s

-- | Invert graph for part II
invertEdges :: Graph -> Graph
invertEdges = M.foldrWithKey consumeAdj M.empty
  where consumeAdj k xs mb = foldr (\x mm -> M.insertWith (++) x [k] mm) mb xs

invertEdges_ :: State DState ()
invertEdges_ = do graph %= invertEdges

main :: IO ()
main = do input <- readFile "data/day12.txt"
          let st = buildState 'S' input
          let result = getLengthToEnd $ execState dijkstra st
          putStrLn $ "length S->E (part I): " ++ show result
          -- for part II, use E as start and invert graph edges
          let st' = buildState 'E' input
          let ft = execState (invertEdges_ >> dijkstra) st'
          let result' = shortestLengthToA ft
          putStrLn $ "closest length to 'a' (part II): " ++ show result'
