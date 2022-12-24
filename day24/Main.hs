{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad.State (State, evalState)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Data.Set as S
import Lens.Micro.Platform ((%=), (.=), (+=), (<~), makeLenses, use)


type Vec2 = (Int, Int)
data Dir = N | E | S | W deriving (Enum, Eq, Show)

data IterState = IterState { _numBliz       :: Int
                           , _blizPos       :: M.Map Int Vec2
                           , _blizDir       :: M.Map Int Dir
                           , _start         :: Vec2
                           , _end           :: Vec2
                           , _target        :: Vec2
                           , _epoch         :: Int
                           , _valley        :: S.Set Vec2
                           , _pos           :: [Vec2]
                           , _nrun          :: Int
                           } deriving Show
makeLenses ''IterState

initState :: ([Vec2], [(Vec2, Dir)]) -> Bool -> IterState
initState (vs, bs) isPartII =
  IterState { _numBliz = length bs
            , _blizPos = M.fromList $ zip [1..] $ map fst bs
            , _blizDir = M.fromList $ zip [1..] $ map snd bs
            , _start   = minimum vs
            , _end     = maximum vs
            , _target  = maximum vs
            , _epoch   = 0
            , _valley  = S.fromList vs
            , _pos     = [minimum vs]
            , _nrun    = if isPartII then 1 else 3
            }
parseValley :: String -> ([Vec2], [(Vec2, Dir)])
parseValley ls = foldl f ([],[]) $ zip [1..] (lines ls)
  where f (xs,ys) (j,str) = let (x,y) = parseLine (j,str) in (x++xs, y++ys)

parseLine :: (Int, String) -> ([Vec2], [(Vec2, Dir)])
parseLine (_, [])     = ([], [])
parseLine (j, xs) = foldl f ([],[]) (zip [1..] xs)
  where f t@(ys,zs) (i, c)
          | c == '.'  = ((i,j):ys, zs)
          | c == '^'  = ((i,j):ys, ((i,j),N):zs)
          | c == '<'  = ((i,j):ys, ((i,j),W):zs)
          | c == '>'  = ((i,j):ys, ((i,j),E):zs)
          | c == 'v'  = ((i,j):ys, ((i,j),S):zs)
          | otherwise = t

run :: State IterState Int
run = do isDone <- checkIfDone
         if isDone
           then use epoch
           else do epoch += 1
                   n <- use numBliz
                   mapM_ moveBlizzard [1..n]
                   updatePos
                   run

checkIfDone :: State IterState Bool
checkIfDone = do t <- use target
                 targetInReach <- ((t `S.member`) . S.fromList) <$> use pos
                 n <- use nrun
                 case (targetInReach, n) of
                   (True, 1) -> do pos .= [t]
                                   target <~ use start
                                   nrun += 1
                                   return False
                   (True, 2) -> do pos .= [t]
                                   target <~ use end
                                   nrun += 1
                                   return False
                   (True, _) -> return True
                   _         -> return False

updatePos :: State IterState ()
updatePos = do xs <- use pos
               bs <- (S.fromList . M.elems) <$> use blizPos
               new <- concat <$> mapM (getNeighs bs) xs
               pos .= (S.toList . S.fromList) new

getNeighs :: (S.Set Vec2) -> Vec2 -> State IterState [Vec2]
getNeighs bs p = do vs <- use valley
                    return $ filter (isValid vs bs) $ ns p
  where ns (x,y) = [(x,y), (x+1,y),(x-1,y),(x,y-1),(x,y+1)]
        isValid vs' bs' p' = (not $ p' `elem` bs') && (p' `S.member` vs')

moveBlizzard :: Int -> State IterState ()
moveBlizzard n = do p <- (fromJust . M.lookup n) <$> use blizPos
                    d <- (fromJust . M.lookup n) <$> use blizDir
                    v <- use valley
                    blizPos %= M.insert n (nextPos v p d)

nextPos :: S.Set Vec2 -> Vec2 -> Dir -> Vec2
nextPos s v d = if inbound then next else goUntilWall s v d'
  where next = advancePos v d
        inbound = next `S.member` s
        d' = toEnum . (`mod` 4) . (+2) . fromEnum $ d

advancePos :: Vec2 -> Dir -> Vec2
advancePos (x,y) N = (x, y-1)
advancePos (x,y) S = (x, y+1)
advancePos (x,y) E = (x+1, y)
advancePos (x,y) W = (x-1, y)

goUntilWall :: S.Set Vec2 -> Vec2 -> Dir -> Vec2
goUntilWall s v d = if inbound then goUntilWall s next d else v
  where next = advancePos v d
        inbound = next `S.member` s

main :: IO ()
main = do
  vs <- parseValley <$> readFile "data/day24.txt"
  putStrLn $ "part I:  " ++ (show $ evalState run (initState vs False))
  putStrLn $ "part II: " ++ (show $ evalState run (initState vs True))
