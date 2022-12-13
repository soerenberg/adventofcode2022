{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad.State.Lazy (State, evalState)
import qualified Data.Set as S
import Lens.Micro.Platform ((%=), makeLenses, use)

data St = St {_counter :: Int, _letters :: String} deriving Show
makeLenses ''St

firstMarker :: State St Int
firstMarker = do xs <- take 4 <$> use letters
                 letters %= tail
                 c <- use counter
                 case (<4) . S.size . S.fromList $ xs of
                   False -> return c
                   _ -> do counter %= (+1)
                           firstMarker

buildState :: String -> St
buildState = St 4

main :: IO ()
main = do input <- readFile "data/day06.txt"
          let x = (evalState firstMarker) . buildState $ input
          putStrLn $ "part I: " ++ show x
