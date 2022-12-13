{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad.State.Lazy (State, evalState)
import qualified Data.Set as S
import Lens.Micro.Platform ((%=), makeLenses, use)

data St = St {_counter :: Int, _letters :: String} deriving Show
makeLenses ''St

firstMarker :: Int -> State St Int
firstMarker n = do xs <- take n <$> use letters
                   letters %= tail
                   c <- use counter
                   case (<n) . S.size . S.fromList $ xs of
                     False -> return c
                     _ -> do counter %= (+1)
                             firstMarker n

getFirstMarker :: Int -> String -> Int
getFirstMarker m = (evalState $ firstMarker m) . (St m)

main :: IO ()
main = do input <- readFile "data/day06.txt"
          putStrLn $ "part  I: " ++ (show $ getFirstMarker 4 input)
          putStrLn $ "part II: " ++ (show $ getFirstMarker 14 input)
