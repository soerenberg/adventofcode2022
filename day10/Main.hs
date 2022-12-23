{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad.State.Lazy (State, runState)
import Data.Either (fromRight)
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
                   , _inProcess    :: Bool
                   , _summand      :: Int
                   , _val          :: Int
                   , _signals      :: [Int]
                   , _crt          :: String
                   , _cmds         :: [Cmd]
                   } deriving Show
makeLenses ''IterState

initState :: [Cmd] -> IterState
initState xs = S 0 False 0 1 [] (replicate 240 '.') xs

iter :: State IterState Int
iter = do isDone <- null <$> use cmds
          if isDone
            then sum <$> use signals
            else iter'

iter' :: State IterState Int
iter' =  do epoch %= (+1)
            isSignal <- (\x -> (x-20) `mod` 40 == 0 && x <= 220) <$> use epoch
            (e, v) <- (,) <$> use epoch <*> use val
            drawPixel
            if isSignal then signals %= (e*v:) else return ()
            inProc <- use inProcess
            if inProc
              then finishAdd
              else use cmds >>= (startNextCmd . head)
            iter

startNextCmd :: Cmd -> State IterState ()
startNextCmd Noop = cmds %= tail
startNextCmd (Add n) = do inProcess .= True
                          cmds %= tail
                          summand .= n

finishAdd :: State IterState ()
finishAdd = do inProcess .= False
               s <- use summand
               val %= ((+) s)

drawPixel :: State IterState ()
drawPixel = do --e <- use epoch
               --v <- use val
               (e, v) <- (,) <$> use epoch <*> use val
               let e' = e - 40 * (div (e-1) 40)
               if v <= e' && e' <= v + 2
                 then crt %= (\(l,r) -> l ++ ('#':tail r)) . (splitAt (e-1))
                 else return ()

drawCrt :: String -> IO ()
drawCrt [] = putStrLn ""
drawCrt xs = putStrLn l >> drawCrt r
  where (l, r) = splitAt 40 xs

main :: IO ()
main = do
  input <- pack <$> readFile "data/day10.txt"
  let cs = fromRight [] $ parse (many cmd) "" input
  let (r, fs) = runState iter (initState cs)
  putStrLn $ "part I: " ++ show r
  drawCrt $ _crt fs
