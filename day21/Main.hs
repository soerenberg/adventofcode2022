module Main (main) where

import Data.Either (fromRight)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Data.Text (pack)
import Text.Parsec.Text (Parser)
import Text.Parsec (
  (<|>), char, digit, letter, many, many1, oneOf, parse)


data AST = Val Int
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         | Var
         deriving (Eq, Show)

buildAST :: [(String, Cmd)] -> AST
buildAST xs = buildAST' "root" $ M.fromList xs

buildAST' :: String -> (M.Map String Cmd) -> AST
buildAST' name m = build . fromJust . (M.lookup name) $  m
  where m' = M.delete name m
        build (N x)        = Val x
        build (Cmd l op r) = op (buildAST' l m') (buildAST' r m')
        build Human        = Var

evalNode :: AST -> Int
evalNode (Val x)   = x
evalNode (Add x y) = (+) (evalNode x) (evalNode y)
evalNode (Sub x y) = (-) (evalNode x) (evalNode y)
evalNode (Mul x y) = (*) (evalNode x) (evalNode y)
evalNode (Div x y) = div (evalNode x) (evalNode y)

-- intermediate representation, since lines are not ordered wrt to final AST
data Cmd = N Int
         | Cmd String (AST -> AST -> AST) String
         | Human

cmd :: Parser (String, Cmd)
cmd = do name <- word <* char ':' <* spaces
         c <- (N <$> int) <|> op
         return (name, c)
  where word = many1 letter <* spaces
        op = do l <- word
                f <- add <|> sub <|> mul <|> div'
                r <- word
                return $ Cmd l f r
        int = read <$> many1 digit <* spaces
        add = char '+' >> spaces >> return Add
        sub = char '-' >> spaces >> return Sub
        mul = char '*' >> spaces >> return Mul
        div' = char '/' >> spaces >> return Div
        spaces = many $ oneOf " \n"

data Solution = Num Int         -- const value
              | F (Int -> Int)  -- f(x)=y => using Var=y will evaluate to x

evalSolution :: AST -> Solution
evalSolution Var       = F id
evalSolution (Val x)   = Num x
evalSolution (Add x y) = combineAdd (evalSolution x) (evalSolution y)
evalSolution (Sub x y) = combineSub (evalSolution x) (evalSolution y)
evalSolution (Mul x y) = combineMul (evalSolution x) (evalSolution y)
evalSolution (Div x y) = combineDiv (evalSolution x) (evalSolution y)

combineAdd :: Solution -> Solution -> Solution
combineAdd (Num x) (Num y) = Num $ (+) x y
combineAdd (F f) (Num y) = F $ f . (subtract y)
combineAdd (Num x) (F f) = F $ f . (subtract x)

combineSub :: Solution -> Solution -> Solution
combineSub (Num x) (Num y) = Num $ (-) x y
combineSub (F f) (Num y) = F $ f . (+y)
combineSub (Num x) (F f) = F $ f . ((-) x)

combineMul :: Solution -> Solution -> Solution
combineMul (Num x) (Num y) = Num $ (*) x y
combineMul (F f) (Num y) = F $ f . (flip div y)
combineMul (Num x) (F f) = F $ f . (flip div x)

combineDiv :: Solution -> Solution -> Solution
combineDiv (Num x) (Num y) = Num $ div x y
combineDiv (F f) (Num y) = F $ f . (*y)
combineDiv (Num x) (F f) = F $ f . (div x)

apply :: Solution -> Int -> Int
apply (F f) = f

changeRoot :: AST -> AST
changeRoot (Add l r) = Sub l r
changeRoot (Mul l r) = Sub l r
changeRoot (Div l r) = Sub l r
changeRoot x = x

main :: IO ()
main = do
  -- part I: parse AST and recursively evaluate root node
  input <- pack <$> readFile "data/day21.txt"
  let cmds =  fromRight [] $ parse (many cmd) "" input
  let r = evalNode . buildAST $ cmds
  putStrLn $ "part I: " ++ (show $ r)

  -- part II: change root node to subtraction so we are looking for roots when
  -- solving for the human value. Then we rebuild the AST by changing human.
  let ast = changeRoot . (buildAST' "root") . (M.insert "humn" Human) . (M.fromList) $ cmds
  let r' = evalSolution ast
  putStrLn $ "part II : " ++ (show $ apply r' 0)
