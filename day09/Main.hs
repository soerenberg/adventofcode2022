{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad (foldM)
import Control.Monad.State.Lazy (State, evalState)
import Data.Either (fromRight)
import qualified Data.Set as S
import Data.Text (pack)
import Lens.Micro.Platform ((%=), (.=), makeLenses, use)
import Text.Parsec.Text (Parser)
import Text.Parsec (char, digit, many, many1, oneOf, parse)

type Vec2 = (Int, Int)

data IterState = IterState { _tpos      :: [Vec2]
                           , _hpos      :: Vec2
                           , _visited   :: S.Set Vec2
                           , _motions   :: [Vec2 -> [Vec2]]
                           }
makeLenses ''IterState

initState :: Bool -> [(Vec2 -> [Vec2])] -> IterState
initState partII xs =
  IterState { _tpos     = replicate (if partII then 9 else 1) (0, 0)
            , _hpos     = (0, 0)
            , _visited  = S.empty
            , _motions  = xs
            }

simulate :: State IterState Int
simulate = do isDone <- null <$> use motions
              if isDone
                then S.size <$> use visited
                else do f <- head <$> use motions
                        motions %= tail
                        hs <- f <$> use hpos
                        hpos .= last hs
                        t <- use tpos
                        newts <- foldM follow t hs
                        tpos .= newts
                        simulate

follow :: [Vec2] -> Vec2 -> State IterState [Vec2]
follow [] _ = return []
follow (t:ts) h = do let new_t = calcTail h t
                     visited %= if null ts then S.insert new_t else id
                     (new_t:) <$> follow ts new_t

motion :: Parser (Vec2 -> [Vec2])
motion = toMotion <$> dir <*> units
  where dir = oneOf "LRUD" <* char ' '
        units = read <$> many1 digit <* char '\n'

toMotion :: Char -> Int -> (Vec2 -> [Vec2])
toMotion 'R' n = (\(x, y) -> [(x + i, y) | i <- [1..n]])
toMotion 'L' n = (\(x, y) -> [(x - i, y) | i <- [1..n]])
toMotion 'U' n = (\(x, y) -> [(x, y - i) | i <- [1..n]])
toMotion 'D' n = (\(x, y) -> [(x, y + i) | i <- [1..n]])

calcTail :: Vec2 -> Vec2 -> Vec2
calcTail h@(hx,hy) t@(tx,ty)
  | h == t               = (hx, hy)
  | (hx==tx) && (hy<ty)  = (hx, hy+1)
  | (hx==tx)             = (hx, hy-1)
  | (hx<tx)  && (hy==ty) = (hx+1, hy)
  |             (hy==ty) = (hx-1, hy)
  | abs (hx-tx) == 1 && abs (hy-ty) == 1 = (tx, ty)
  | abs (hx-tx) > 1  && abs (hy-ty) == 1 = (hx - signum (hx-tx), hy)
  | abs (hx-tx) == 1 && abs (hy-ty) > 1  = (hx, hy - signum (hy-ty))
  | abs (hx-tx) == abs (hy-ty) = (hx - signum (hx-tx), hy - signum (hy-ty))

main :: IO ()
main = do input <- pack <$> readFile "data/day09.txt"
          let ms = fromRight [] $ parse (many motion) "" input
          let r = evalState simulate (initState False ms)
          putStrLn $ "part I: " ++ show r
          let r' = evalState simulate (initState True ms)
          putStrLn $ "part II: " ++ show r'
