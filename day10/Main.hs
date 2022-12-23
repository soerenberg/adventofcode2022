{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad.State.Lazy (State, evalState)
import Data.Either (fromRight)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Text (pack)
import Lens.Micro.Platform ((%=), (.=), makeLenses, use)
import Text.Parsec.Text (Parser)
import Text.Parsec ((<|>), char, digit, many, many1, oneOf, option, string, parse)


data Cmd = Add Int | Noop deriving Show

cmd :: Parser Cmd
cmd = add <|> noop
  where add = string "addx " >> (Add <$> sint) <* spaces
        noop = string "noop" >> spaces >> return Noop <* spaces
        spaces = many $ oneOf " \n"
        sint = read <$> ((:) <$> (option ' ' (char '-')) <*> (many1 digit))

data IterState = S { _epoch        :: Int
                   , _addExec      :: Int
                   , _summand      :: Int
                   , _val          :: Int
                   , _signals      :: [Int]
                   , _cmds         :: [Cmd]
                   } deriving Show
makeLenses ''IterState

initState :: [Cmd] -> IterState
initState xs = S 0 0 0 1 [] xs

play :: State IterState Int
play =
  do isDone <- null <$> use cmds
     if isDone
       then sum <$> use signals
       else do epoch %= (+1)
               isSignal <- (\x -> (x-20) `mod` 40 == 0 && x <= 220) <$> use epoch
               v <- use val
               e <- use epoch
               if isSignal then signals %= (e*v:) else return ()
               inProc <- use addExec
               case inProc of
                 2 -> do addExec %= subtract 1
                         play
                 1 -> do addExec %= subtract 1
                         s <- use summand
                         val %= ((+) s)
                         play
                 _ -> do c <- head <$> use cmds
                         case c of
                           Noop -> do cmds %= tail
                                      play
                           (Add n) -> do addExec .= 1
                                         cmds %= tail
                                         summand .= n
                                         play

main :: IO ()
main = do
  input <- pack <$> readFile "data/day10.txt"
  let cs = fromRight [] $ parse (many cmd) "" input

  let r = evalState play (initState cs)
  putStrLn $ "part I: " ++ show r
