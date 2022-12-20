module Main (main) where

import Data.Maybe (fromJust)
import qualified Data.List as L

type Vec2 = (Int, Int)

move :: Int -> [Vec2] -> Vec2 -> [Vec2]
move l xs p@(x,_) = moveRightAt xs i (x `mod` (l-1))
  where i = fromJust $ p `L.elemIndex` xs

moveRightAt :: [Vec2] -> Int -> Int -> [Vec2]
moveRightAt xs i n = [x] ++  bs ++ as
  where (ys, zs) = L.splitAt i xs
        (as, bs) = L.splitAt n  $ tail zs ++ ys
        x = head zs

moves :: [Vec2] -> [Vec2]
moves l = foldl (move (length l)) l l

getAt :: [a] -> Int -> Int -> a
getAt xs l i = xs !! (i `mod` l)

coordSum :: [Vec2] -> Int -> Int
coordSum xs l = sum $ map (fst . (getAt (zs++ys) l)) [1000, 2000, 3000]
  where (ys, zs) = splitAt i xs
        i = fromJust $ 0 `L.elemIndex` (map fst xs)

main :: IO ()
main = do
  input <- readFile "data/day20.txt"
  let xs = map read $ lines input
  let r = coordSum (moves $ zip xs [0..]) (length xs)
  putStrLn $ "part I: " ++ (show r)
