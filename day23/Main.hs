{-# LANGUAGE TemplateHaskell, TupleSections #-}
module Main (main) where

import Control.Monad.State (State, evalState, execState)
import qualified Data.Map as M
import qualified Data.Set as S
import Lens.Micro.Platform ((%=), (.=), makeLenses, use)


type Vec2 = (Int, Int)

parseElves :: String -> [Vec2]
parseElves xs = concat . (map parseElves') . (zip [1..]) . lines $ xs

parseElves' :: (Int, String) -> [Vec2]
parseElves' (_, [])     = []
parseElves' (i, xs) = (map ((,i) . fst)) . (filter ((=='#') . snd)) . (zip [1..]) $ xs

bounds :: S.Set Vec2 -> (Vec2, Vec2)
bounds es = ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where xs = map fst l
        ys = map snd l
        l = S.toList es

data Dir = N | S | W | E deriving (Enum, Show)

data IterState = IterState { _proposals   :: M.Map Vec2 Vec2
                           , _proposalCnt :: M.Map Vec2 Int
                           , _isOk        :: [Bool]
                           , _elves       :: S.Set Vec2
                           , _epoch       :: Int
                           , _dir         :: Dir
                           } deriving Show
makeLenses ''IterState

initState :: S.Set Vec2 -> IterState
initState xs = IterState M.empty M.empty [False] xs 0 E

advance :: State IterState ()
advance = do proposals .= M.empty
             proposalCnt .= M.empty
             isOk .= []
             epoch %= (+1)
             dir %= toEnum . (`mod` 4) . (+1) . (fromEnum)

run :: Bool -> State IterState Int
run partII = do
  isDone <- if partII then and <$> use isOk else (==10) <$> use epoch
  if isDone
    then do es <- use elves
            let ((x,y), (a,b)) = bounds es
            return $ (a-x+1) * (b-y+1) - (S.size es)
    else do advance
            es <- use elves
            dirs <- getDirs <$> use dir
            mapM_ (proposeElf dirs) (S.toList es)
            mapM_ moveElf (S.toList es)
            run partII

moveElf :: Vec2 -> State IterState ()
moveElf v =
  do mp <- (M.lookup v) <$> use proposals
     case mp of
       Nothing -> return ()
       (Just p) -> do cnt <- (M.findWithDefault 0 p) <$> use proposalCnt
                      if cnt == 1
                        then do elves %= S.delete v
                                elves %= S.insert p
                        else return ()

proposeElf :: [Dir] -> Vec2 -> State IterState ()
proposeElf dirs v = do es <- use elves
                       let a = map (canMove es v) dirs
                       let allGood = and . (map fst) $ a
                       let noneGood = not . or . (map fst) $ a
                       isOk %= (allGood:)
                       if allGood || noneGood
                         then return ()
                         else do let p = snd . head . (filter fst) $ a
                                 proposals %= M.insert v p
                                 proposalCnt %= M.insertWith (+) p 1

getDirs :: Dir -> [Dir]
getDirs = (\n -> map (toEnum . (`mod` 4) . (+n)) [0..3]) . fromEnum

canMove :: S.Set Vec2 -> Vec2 -> Dir -> (Bool, Vec2)
canMove xs (x,y) N = (xs `notContains` [(x-1,y-1),(x,y-1),(x+1,y-1)], (x,y-1))
canMove xs (x,y) S = (xs `notContains` [(x-1,y+1),(x,y+1),(x+1,y+1)], (x,y+1))
canMove xs (x,y) E = (xs `notContains` [(x+1,y-1),(x+1,y),(x+1,y+1)], (x+1,y))
canMove xs (x,y) W = (xs `notContains` [(x-1,y-1),(x-1,y),(x-1,y+1)], (x-1,y))

notContains :: Ord a => S.Set a -> [a] -> Bool
notContains xs = not . or . (map (`S.member` xs))

main :: IO ()
main = do
  input <- readFile "data/day23.txt"
  let s = S.fromList $ parseElves input
  putStrLn $ "part I: " ++ (show $ evalState (run False) (initState s))
  let fs = execState (run True) (initState s)
  putStrLn $ "part II: " ++ (show  $ _epoch fs)
