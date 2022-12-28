{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad.State.Lazy (State, evalState)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Lens.Micro.Platform ((%=), (<<%=), makeLenses , use)


type Vec2 = (Int, Int)
type Epoch = Int
type Level = Int

data CacheState = CS { rockIdx    :: Int
                     , jetIdx     :: Int
                     , window     :: S.Set Vec2
                     } deriving (Eq, Ord, Show)

data S = S { _cave      :: S.Set Vec2
           , _rockIndex :: Int
           , _jet       :: M.Map Int Int
           , _jetCount  :: Int
           , _jetNum    :: Int
           , _level     :: Int
           , _epoch     :: Int
           , _cache     :: M.Map CacheState (Epoch, Level)
           } deriving Show
makeLenses ''S

initState :: String -> S
initState s = S { _cave      = S.empty
                , _rockIndex = 0
                , _jet       = M.fromList $ zip [0..] xs
                , _jetCount  = 0
                , _jetNum    = length s
                , _level     = 0
                , _epoch     = 0
                , _cache     = M.empty
                }
  where xs = map (\c -> if c=='<' then (-1) else 1) s

data Rock = Rock { cells  :: [Vec2]
                 , kind   :: Int
                 , boundL :: Int
                 , boundR :: Int
                 , boundT :: Int
                 , boundB :: Int
                 } deriving (Show, Eq)

popJet :: State S Int
popJet = do n <- use jetNum
            c <- use jetCount
            j <- (fromJust . (M.lookup c)) <$> use jet
            jetCount %= (`mod` n) . (+1)
            pure j

pop3Jets :: State S (Int, Int, Int)
pop3Jets = do n <- use jetNum
              c <- use jetCount
              js <- use jet
              let t = ( fromJust $ M.lookup (c `mod` n) js
                      , fromJust $ M.lookup ((c+1) `mod` n) js
                      , fromJust $ M.lookup ((c+2) `mod` n) js)
              jetCount %= (`mod` n) . (+3)
              pure t

vshift :: Int -> Rock -> Rock
vshift z (Rock cs k l r t b) = Rock (map (\(x,y)->(x+z,y)) cs) k (l+z) (r+z) t b

-- we can optimize the first 3 movements depending on the rocks
-- during the first 3 moves we don't need to check collision with other rocks
moveInitRock :: Rock -> State S Rock
moveInitRock r =
  do (s1, s2, s3) <- pop3Jets
     case (s1+s2, s1+s2+s3, kind r) of
       (-2, -1, _) -> pure $ vshift (-1) r
       (-2, -3, _) -> pure $ vshift (-2) r
       (0, -1,  _) -> pure $ vshift (-1) r
       (0, 1,   _) -> pure $ vshift 1    r
       (2, 3,   0) -> pure $ vshift 1    r
       (2, 3,   1) -> pure $ vshift 2    r
       (2, 3,   2) -> pure $ vshift 2    r
       (2, 3,   _) -> pure $ vshift 3    r
       (2, 1,   0) -> pure $ r
       _           -> pure $ vshift 1    r   -- case (2,1,_)

isFeasible :: Rock -> State S Bool
isFeasible (Rock cs _ bl br _ b) =
  do cv <- use cave
     let notOcc = not . or . (map (`S.member` cv)) $ cs
     return $ bl >= 1 && br <= 7 && notOcc && b > 0

jetRock :: Rock -> State S Rock
jetRock r = do r' <- flip vshift r <$> popJet
               feas <- isFeasible r'
               pure $ if feas then r' else r

fall :: Rock -> State S (Rock, Bool)
fall r = do let r' = r { cells = map (\(x,y)->(x,y-1)) $ cells r
                       , boundT = boundT r - 1
                       , boundB = boundB r - 1}
            feas <- isFeasible r'
            pure $ if feas then (r', True) else (r, False)

moveRock :: Rock -> State S Rock
moveRock r = do (r', fallen) <- jetRock r >>= fall
                if fallen then moveRock r' else pure r'

putRock :: Rock -> State S ()
putRock (Rock cs _ _ _ t _) =
  do cave %= (\c -> foldr S.insert c cs)
     level %= (t `max`)

getCacheElt :: State S CacheState
getCacheElt = do r <- use rockIndex
                 j <- use jetCount
                 l <- use level
                 w <- S.filter ((>(l-64)) . snd) <$> use cave
                 pure $ CS r j (S.map (\(x,y) -> (x, y-l)) w)

updateCache :: State S (Maybe (Epoch, Level))
updateCache = do ch <- getCacheElt
                 lu <- (M.lookup ch) <$> use cache
                 e <- use epoch
                 l <- use level
                 cache %= M.insert ch (e, l)
                 pure lu

useCycleToFinish :: Int -> (Epoch, Level) -> State S Int
useCycleToFinish m (e, l) =
  do currentEpoch <- use epoch
     currentLevel <- use level
     let cycleLength = currentEpoch - e
     let cyclesRemaining = div (m - currentEpoch) cycleLength
     remainder <- simLevel (m - cyclesRemaining * cycleLength + 1) False
     pure $ cyclesRemaining * (currentLevel - l) + remainder

simLevel :: Int -> Bool -> State S Int
simLevel maxEpoch useCache =
  do done <- (>=maxEpoch) <$> (epoch <<%= (+1))
     if done
       then use level
       else do cachedEpoch <- updateCache
               case (useCache, cachedEpoch) of
                 (True, Just t) -> useCycleToFinish maxEpoch t
                 _ -> do rc <- rockIndex <<%= (`mod` 5) . (+1)
                         r <- rocks rc <$> use level >>= moveInitRock >>= moveRock
                         putRock r
                         simLevel maxEpoch useCache

-- Since we don't need to test collison during the first 3 moves (jet/fall), we
-- spawn a rock 3 cells below than their actual position. We then save
-- decrementing the rocks y position 3 times.
rocks :: Int -> Int -> Rock
rocks 0 y = Rock [(i, y+1) | i <- [3..6]]                           0 3 6 (y+1) (y+1)
rocks 1 y = Rock [(4, y+3), (3, y+2), (4, y+2), (5, y+2), (4, y+1)] 1 3 5 (y+3) (y+1)
rocks 2 y = Rock [(5, y+3), (5, y+2), (3, y+1), (4, y+1), (5, y+1)] 2 3 5 (y+3) (y+1)
rocks 3 y = Rock [(3, y+i) | i <- [1..4]]                           3 3 3 (y+4) (y+1)
rocks _ y = Rock [(3, y+2), (4, y+2), (3, y+1), (4, y+1)]           4 3 4 (y+2) (y+1)

main :: IO ()
main = do input <- ((!!0) . lines) <$> readFile "data/day17.txt"
          let r = evalState (simLevel 2022 True) (initState input)
          putStrLn $ "part I : " ++ (show $ r)

          let r' = evalState (simLevel 1000000000000 True) (initState input)
          putStrLn $ "part II: " ++ (show r')
