module Main (main) where

import Control.Monad.State.Lazy (State, evalState, get)
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Data.Text (pack)
import Text.Parsec.Text (Parser)
import Text.Parsec (
  (<|>), char, digit, letter, many, many1, oneOf, parse)

data Cmd = N Int
         | Add String String
         | Sub String String
         | Mul String String
         | Div String String
         deriving Show

cmd :: Parser (String, Cmd)
cmd = do name <- word <* char ':' <* spaces
         c <- (N <$> int) <|> op
         return (name, c)
  where word = many1 letter <* spaces
        op = do l <- word
                f <- add <|> sub <|> mul <|> div'
                r <- word
                return $ f l r
        int = read <$> many1 digit <* spaces
        add = char '+' >> spaces >> return Add
        sub = char '-' >> spaces >> return Sub
        mul = char '*' >> spaces >> return Mul
        div' = char '/' >> spaces >> return Div
        spaces = many $ oneOf " \n"

type Env = M.Map String Cmd

eval :: String -> State Env Int
eval var = do  s <- get
               let c = fromJust . (M.lookup var) $ s
               eval' c

eval' :: Cmd -> State Env Int
eval' (N x) = return x
eval' (Add l r) = (+) <$> (eval l) <*> (eval r)
eval' (Sub l r) = (-) <$> (eval l) <*> (eval r)
eval' (Mul l r) = (*) <$> (eval l) <*> (eval r)
eval' (Div l r) = div <$> (eval l) <*> (eval r)

main :: IO ()
main = do
  input <- pack <$> readFile "data/day21.txt"
  let cmds = M.fromList $ fromRight [] $ parse (many cmd) "" input
  let r = evalState (eval "root") cmds
  putStrLn $ "part I: " ++ (show $ r)
