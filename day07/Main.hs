{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Data.Either (fromRight)
import qualified Data.Map as M
import Data.Text (pack)
import Text.Parsec.Text (Parser)
import Text.Parsec (
  (<|>), char, digit, letter, many, many1, oneOf, string, try, parse)


data FileTree = Folder (M.Map String FileTree) | File Int deriving Show

type Path = [String]

-- | Parsing
parseTree :: Parser FileTree
parseTree = do xs <- many $ cd <|> ls
               let t = (Folder M.empty, [])
               let (t', _) = foldl (\x f -> f x) t xs
               return t'

cd :: Parser ((FileTree, Path) -> (FileTree, Path))
cd = do _ <- try $ string "$ cd "
        f <- name <* eol
        case f of
          "/"  -> return (\(ft, _) -> (ft, []))
          ".." -> return (\(ft, p) -> (ft, if null p then [] else init p))
          x    -> return (\(ft, p) -> (ft, p ++ [x]))

ls :: Parser ((FileTree, Path) -> (FileTree, Path))
ls = do _ <- try $ string "$ ls" <* eol
        xs <- many lsOutput
        return (\(ft, p) -> (foldl (\q (n, r) -> insert r (p++[n]) q) ft xs, p))

insert :: FileTree -> Path -> FileTree -> FileTree
insert n [x] (Folder m) = Folder $ M.insertWith seq x n m
insert n (x:xs) (Folder m) = Folder $ M.insert x subs m
  where subs = insert n xs t'
        t' = M.findWithDefault (Folder M.empty) x m
insert _ [] f = f          -- should never happen
insert _ _ f@(File _) = f  -- should never happen

name :: Parser String
name = many1 $ letter <|> oneOf "./"

lsOutput :: Parser (String, FileTree)
lsOutput = file <|> dir
  where file = do s <- read <$> many1 digit <* char ' '
                  n <- name <* eol
                  return (n, File s)
        dir = do n <- string "dir " >> name <* eol
                 return (n, Folder M.empty)

eol :: Parser Char
eol = char '\n'

size :: FileTree -> Int
size (File n) = n
size (Folder xs) = sum . (M.map size) $ xs

folderSizes :: FileTree -> [Int]
folderSizes (File _) = []
folderSizes x@(Folder xs) = (size x):(M.elems xs >>= folderSizes)

main :: IO ()
main = do input <- pack <$> readFile "data/day07.txt"
          let sizes = fromRight [] $ folderSizes <$> parse parseTree "" input
          let r = sum . (filter (<=100000)) $ sizes
          putStrLn $ "part  I: " ++ show r
