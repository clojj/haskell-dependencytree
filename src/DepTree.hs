module DepTree
  ( getPaths
  , Tree
  ) where

import           Data.List (findIndex, findIndices, foldl, foldl', foldr, map,
                            reverse, take)

type Name = String

type TreeItem = (Int, String)

type Tree = [TreeItem]

findItem :: Name -> Tree -> [Int]
findItem name tree = findIndices (\e -> (snd e) == name) tree

treesToItem :: Name -> Tree -> [Tree]
treesToItem name tree = do
  case findItem name tree of
    []      -> []
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
