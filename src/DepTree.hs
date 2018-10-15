{-# LANGUAGE OverloadedStrings #-}

module DepTree
  ( getPaths
  , toTree
  , Tree
  ) where

import Data.List (findIndex, findIndices, foldl, foldl', foldr, map, reverse, take)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Char (isAlpha)

type Name = T.Text

type TreeItem = (Int, T.Text)

type Tree = [TreeItem]

findItem :: Name -> Tree -> [Int]
findItem name tree = findIndices (\e -> (snd e) == name) tree

treesToItem :: Name -> Tree -> [Tree]
treesToItem name tree = do
  case findItem name tree of
    [] -> []
    indexes -> foldl' (\list n -> (take (n + 1) tree) : list) [] indexes

rootPath :: Tree -> [Name]
rootPath tree = snd $ foldl' fun (0, []) tree

fun :: (Int, [Name]) -> TreeItem -> (Int, [Name])
fun acc@(level, names) item@(itemLevel, itemName)
  | itemLevel < level || names == [] = (itemLevel, (itemName : names))
  | otherwise = acc

getPaths :: Name -> Tree -> [[Name]]
getPaths name tree = do
  let trees = treesToItem name tree
   in map (rootPath . reverse) trees

toTree :: FilePath -> IO Tree
toTree file = do
  content <- TIO.readFile file
  let lines = T.lines content
   in return $ map lineToTreeItem lines

lineToTreeItem :: T.Text -> TreeItem
lineToTreeItem line =
  let (pre, post) = T.break isAlpha line
   in (T.length pre, post)