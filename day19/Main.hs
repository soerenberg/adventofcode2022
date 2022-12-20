{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad.State.Lazy (State, evalState, get)
import Data.Either (fromRight)
import qualified Data.Set as S
import Data.Text (pack)
import Lens.Micro.Platform ((%=), makeLenses, use, zoom)
import Text.Parsec.Text (Parser)
import Text.Parsec ((<|>), digit, letter, many, many1, oneOf, parse)


line :: Parser BluePrint
line = do _ <- nondigit >> int >> nondigit
          oreCost <- int <* nondigit
          clayCost <- int <* nondigit
          obsCost <- (,) <$> (int <* nondigit) <*> (int <* nondigit)
          geoCost <- (,) <$> (int <* nondigit) <*> (int <* nondigit)
          return $ BluePrint { oreRobotCost = oreCost
                             , clayRobotCost = clayCost
                             , obsidianRobotCost = obsCost
                             , geodeRobotCost = geoCost
                             , maxOreCost = maximum [oreCost, clayCost, fst obsCost, fst geoCost]
                             , maxClayCost = snd obsCost
                             , maxObsidianCost = snd geoCost
                             }
  where nondigit = many $ letter <|> oneOf " :.\n"
        int = read <$> many1 digit

data BluePrint = BluePrint { oreRobotCost      :: Int        -- ore
                           , clayRobotCost     :: Int        -- ore
                           , obsidianRobotCost :: (Int, Int) -- ore, clay
                           , geodeRobotCost    :: (Int, Int) -- ore, obsidian
                           , maxOreCost        :: Int
                           , maxClayCost       :: Int
                           , maxObsidianCost   :: Int
                           } deriving Show

-- this is a quick and dirty non-state monad using solution for testing
-- and comparison
bfs :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool -> Bool -> Bool -> BluePrint -> Int
bfs  0 _ _ _ _ _ _ _ geos _ _ _ _ = geos
bfs mins oreR clayR obsR geoR ore clay obs geos buyMoreOreR buyMoreClayR buyMoreObsR b = maximum nexts
  where nexts = buyGeoRobot ++ buyObsRobot ++ buyClayRobot ++ buyOreRobot ++ idle
        buyOreRobot = if buyMoreOreR && canBuyOreRobot && needOreRobot then [bfs (mins-1) (oreR+1) clayR obsR geoR (ore'-oreOreCost) clay' obs' geos' True needClayRobot needObsRobot b] else []
        canBuyOreRobot  = (oreOreCost <= ore)
        needOreRobot = (oreR * mins + ore < mins * (maxOreCost bp))
        oreOreCost = oreRobotCost b
        buyClayRobot = if buyMoreClayR && canBuyClayRobot && needClayRobot then [bfs (mins-1) oreR (clayR+1) obsR geoR (ore'-clayOreCost) clay' obs' geos' needOreRobot True needObsRobot b] else []
        canBuyClayRobot = (clayOreCost <= ore)
        needClayRobot = (clayR * mins + clay < mins * (maxClayCost bp))
        clayOreCost = clayRobotCost b
        buyObsRobot = if buyMoreObsR && canBuyObsRobot && needObsRobot then [bfs (mins-1) oreR clayR (obsR+1) geoR (ore'-obsOreCost) (clay'-obsClayCost) obs' geos' needOreRobot needClayRobot True b] else []
        canBuyObsRobot  = (obsOreCost <= ore) && (obsClayCost <= clay)
        needObsRobot = (obsR * mins + obs < mins * (maxObsidianCost bp))
        buyGeoRobot = if canBuyGeoRobot then [bfs (mins-1) oreR clayR obsR (geoR+1) (ore'-geoOreCost) clay' (obs'-geoObsCost) geos' buyMoreOreR buyMoreClayR buyMoreObsR b] else []
        canBuyGeoRobot  = (geoOreCost <= ore) && (geoObsCost <= obs)
        idle = [bfs (mins-1) oreR clayR obsR geoR ore' clay' obs' geos' (not canBuyOreRobot && buyMoreOreR) (not canBuyClayRobot && buyMoreClayR) (not canBuyObsRobot && buyMoreObsR) b]
        ore' = ore + oreR
        clay' = clay + clayR
        obs' = obs + obsR
        geos' = geos + geoR
        (obsOreCost, obsClayCost) = obsidianRobotCost b
        (geoOreCost, geoObsCost) = geodeRobotCost b

es' = bfs 24 1 0 0 0 0 0 0 0 True True True bp
es'' = bfs 24 1 0 0 0 0 0 0 0 True True True bp'

fs' =  bfs 32 1 0 0 0 0 0 0 0 True True True bp
fs'' = bfs 32 1 0 0 0 0 0 0 0 True True True bp'

bp = BluePrint { oreRobotCost      = 4
               , clayRobotCost     = 2
               , obsidianRobotCost = (3, 14)
               , geodeRobotCost    = (2, 7)
               , maxOreCost        = 4
               , maxClayCost       = 14
               , maxObsidianCost   = 7
               }

bp' = BluePrint { oreRobotCost      = 2
                , clayRobotCost     = 3
                , obsidianRobotCost = (3, 8)
                , geodeRobotCost    = (3, 12)
                , maxOreCost        = 3
                , maxClayCost       = 8
                , maxObsidianCost   = 12
                }

main :: IO ()
main = do
  putStrLn $ "test (9): " ++ (show es')
  putStrLn $ "test (12): " ++ (show es'')
  putStrLn $ "test (56): " ++ (show fs')
  putStrLn $ "test (62): " ++ (show fs'')
  input <- pack <$> readFile "data/day19.txt"
  let bs = fromRight [] $ parse (many line) "" input
  -- part I
  let gs = map (bfs 24 1 0 0 0 0 0 0 0 True True True) bs
  putStrLn $ "part I: " ++ (show gs)
  let r = sum $ map (uncurry (*)) $ zip [1..] gs
  putStrLn $ "part I: " ++ (show r)

  -- part II
  let gs = map (bfs 32 1 0 0 0 0 0 0 0 True True True) (take 3 bs)
  putStrLn $ "part I: " ++ (show gs)
  let r = product gs
  putStrLn $ "part II: " ++ (show r)
