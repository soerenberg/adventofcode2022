{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad (forM_)
import Control.Monad.State.Lazy (State, evalState)
import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.Set as S
import Lens.Micro.Platform ((%=), (.=), makeLenses, use)


type Vec2 = (Int, Int)

buildGridWith :: (a -> b) -> [[a]] -> [((Int, Int), b)]
buildGridWith f xs = do (y, s) <- zip [1..] xs
                        zipWith (\x c -> ((x, y), f c)) [1..] s

type ElevationMap = M.Map (Int, Int) Int

data IterState = IterState { _visibles :: S.Set Vec2
                           , _maxElev  :: Int
                           , _elevs    :: ElevationMap
                           , _width    :: Int
                           , _height   :: Int
                           }
makeLenses ''IterState

countVisibles :: State IterState Int
countVisibles = do w <- use width
                   h <- use height
                   [(1, y) | y <- [1..h]] `forM_` (epoch right)
                   [(w, y) | y <- [1..h]] `forM_` (epoch left)
                   [(x, 1) | x <- [1..w]] `forM_` (epoch down)
                   [(x, h) | x <- [1..w]] `forM_` (epoch up)
                   vs <- use visibles
                   return $ S.size vs
  where right = (\(x, y) -> (x + 1, y))
        left  = (\(x, y) -> (x - 1, y))
        up    = (\(x, y) -> (x, y - 1))
        down  = (\(x, y) -> (x, y + 1))

epoch :: (Vec2 -> Vec2) -> Vec2 -> State IterState ()
epoch f p = do es <- use elevs
               m <- use maxElev
               let curElev = M.lookup p es
               case curElev of
                 (Just e) -> if e > m
                               then do visibles %= S.insert p
                                       maxElev .= e
                                       epoch f (f p)
                               else epoch f (f p)
                 _ -> maxElev .= -1

initState :: ElevationMap -> IterState
initState m = IterState { _visibles = S.empty
                        , _maxElev  = -1
                        , _elevs    = m
                        , _width    = maximum . (map fst) $ M.keys m
                        , _height   = maximum . (map snd) $ M.keys m }


main :: IO ()
main = do input <- readFile "data/day08.txt"
          let grid = M.fromList $ buildGridWith C.digitToInt $ lines input
          let counts = evalState countVisibles $ initState grid
          putStrLn $ "count (part I): " ++ (show $ counts)
