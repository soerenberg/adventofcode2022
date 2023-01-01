{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad (forM_)
import Control.Monad.State.Lazy (State, evalState)
import Data.Either (fromRight)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (pack)
import Text.Parsec.Text (Parser)
import Text.Parsec (digit, letter, many, many1, oneOf, sepBy, string, parse)
import Lens.Micro.Platform ((%=), makeLenses , use)


-- | Build graph representation
data WGraph k w = WGraph { nodes   :: [k]
                         , edges   :: M.Map k [k]
                         , weights :: M.Map (k, k) w
                         } deriving (Eq, Show)

-- | Extended integers
data ExtNum a = N a | Inf deriving (Eq, Show)
instance (Ord a) => Ord (ExtNum a) where
  compare Inf Inf = EQ
  compare _ Inf = LT
  compare Inf _ = GT
  compare (N a) (N b) = compare a b
instance Functor ExtNum where
  --fmap :: (a -> b) -> f a -> f b
  fmap f (N a) = N $ f a
  fmap _ Inf   = Inf
instance (Num a) => Num (ExtNum a) where
  (+) (N a) (N b) = N $ a + b
  (+) _ _         = Inf
  (*) (N a) (N b) = N $ a * b
  (*) _ _         = Inf
  abs x = abs <$> x
  signum x = signum <$> x
  fromInteger i = N $ fromInteger i
  negate x = negate <$> x

fromN :: a -> (ExtNum a) -> a
fromN _ (N x) = x
fromN x Inf   = x

data DState k w = DS { _graph         :: WGraph k w
                     , _lengths       :: M.Map k (ExtNum w)
                     , _unvisited     :: S.Set k
                     } deriving (Eq, Show)
makeLenses ''DState

dijkstra :: (Ord k, Num w, Ord w) => (WGraph k w) -> k -> (M.Map k (ExtNum w))
dijkstra g a = evalState dijkstra' s
  where s = DS g ls (S.fromList bs)
        ls = (M.fromList $ [(a, N 0)] ++ [(i, Inf) | i <- bs, i /= a])
        bs = nodes g

dijkstra' :: (Ord k, Num w, Ord w) => State (DState k w) (M.Map k (ExtNum w))
dijkstra' = do isDone <- S.null <$> use unvisited
               f isDone
  where f True = use lengths >>= return
        f False = do iter
                     dijkstra'

iter :: (Ord k, Num w, Ord w) => State (DState k w) ()
iter = do (ck, cw) <- findNext
          updateNeighbors ck cw

findNext :: (Ord k, Ord w) => State (DState k w) (k, ExtNum w)
findNext = do ls <- use lengths
              xs <- use unvisited
              let (a, x) = min' xs ls
              unvisited %= S.delete a
              return (a, x)
  where min' zs = (L.minimumBy cmp) . M.toList . (M.filterWithKey $ cond zs)
        cond zs z _ = z `S.member` zs
        cmp (_, a) (_, b) = compare a b

updateNeighbors :: (Ord k, Num w, Ord w) => k -> (ExtNum w) -> State (DState k w) ()
updateNeighbors a x = do ns <- (M.findWithDefault [] a) . edges <$> use graph
                         ns `forM_` (updateNeighbor a x)
                         return ()

updateNeighbor :: (Ord k, Num w, Ord w) => k -> (ExtNum w) -> k -> State (DState k w) ()
updateNeighbor a x b = do wn <- (M.lookup (a, b)) . weights <$> use graph
                          let wn' = fromMaybe Inf $ N <$> wn
                          lengths %= M.insertWith min b (x + wn')


shrinkTo :: (Ord k, Num w, Ord w) => (WGraph k w) -> [k] -> (WGraph k w)
shrinkTo g as = WGraph { nodes = as
                       , edges = M.fromList adjLists
                       , weights = M.fromList $ concat ws
                       }
  where (adjLists, ws) = unzip . (map (uncurry adjAndWeights)) $ dists
        dists = zip as (map (filterNodes . (dijkstra g)) as)
        filterNodes ds = M.filterWithKey (\k _ -> k `elem` as) ds

adjAndWeights :: (Ord k, Num w, Ord w) => k
                                       -> (M.Map k (ExtNum w))
                                       -> ((k, [k]), [((k, k), w)])
adjAndWeights v m = ((v, es), ws)
  where es = M.keys filtered
        ws = map (\(kx, kw) -> ((v, kx), fromN 0 kw)) $ M.toList filtered
        filtered = M.filterWithKey (\k w -> w /= Inf && k /= v) m

type Node = String

line :: Parser (Node, Int, [Node])
line = do n <- word >> word
          r <- read <$> (string "has flow rate=" >> many1 digit)
          _ <- string "; " <* word <* word <* word <* word
          as <- word `sepBy` (string ", ") <* spaces
          return (n, r, as)
  where word = many1 letter <* spaces
        spaces = many $ oneOf "\n "

graphFromRecords :: [(Node, Int, [Node])] ->  (WGraph String Int)
graphFromRecords xs = do let ns = map (\(x, _, _) -> x) xs
                         let es = map (\(x, _, z) -> (x, z)) xs
                         let ws = es >>= (\(x, zs) -> [((x,z), 1) | z <- zs])
                         WGraph ns (M.fromList es) (M.fromList ws)

findOptimalFromGraph :: (WGraph Node Int)
                     -> (M.Map Node Int)
                     -> Bool
                     -> S.Set Node
                     -> Int
                     -> Node
                     -> Int
findOptimalFromGraph g vs elephant unvisits minLeft cn
  | minLeft <= 0  = 0
  | null unvisits = 0
  | otherwise = maximum $ S.map (findOptimalFromGraph' g vs elephant unvisits minLeft cn) unvisits

findOptimalFromGraph' :: (WGraph Node Int)
                      -> (M.Map Node Int)
                      -> Bool
                      -> S.Set Node
                      -> Int
                      -> Node
                      -> Node
                      -> Int
findOptimalFromGraph' g vs elephant unvisits minLeft od nd = max (val + rec) (val + rec2)
  where val = if newTime > 0 then newTime * (fromJust $ M.lookup nd vs) else 0
        rec = findOptimalFromGraph g vs elephant (S.delete nd unvisits) newTime nd
        rec2 = if elephant then 0 else findOptimalFromGraph g vs True (S.delete nd unvisits) 26 "AA"
        moveTime = fromJust . (M.lookup (od, nd)) . weights $ g
        newTime = minLeft - moveTime - 1

findOptimal :: [(Node, Int, [Node])] -> Bool -> Int
findOptimal xs isPartI =
    findOptimalFromGraph g vs isPartI (S.fromList relNodes') m "AA"
  where g = rawGraph `shrinkTo` relNodes
        relNodes = ("AA"):(relNodes')
        relNodes' = map (\(x,_,_) -> x) $ filter (\(_,y,_) -> y > 0) xs
        rawGraph = graphFromRecords xs
        vs = M.fromList $ map (\(x,y,_) -> (x,y)) xs
        m = if isPartI then 30 else 26

main :: IO ()
main = do
  input <- pack <$> readFile "data/day16.txt"

  let recs = fromRight [] $ parse (many1 line) "" input
  putStrLn $ "part  I: " ++ (show $ findOptimal recs True)

  putStrLn $ "part II: " ++ (show $ findOptimal recs False)
