module DepTree (getPaths, Tree) where

import           Data.List (findIndex, findIndices, foldl, foldl', foldr, map,
                            reverse, take)

type Name = String

type TreeItem = (Int, String)

type Tree = [TreeItem]

findItem :: Name -> Tree -> [Int]
findItem n t = findIndices (\e -> (snd e) == n) t

treesToItem :: Name -> Tree -> [Tree]
treesToItem n t = do
  case findItem n t of
    []      -> []
    indexes -> foldl' (\list n -> (take (n + 1) t) : list) [] indexes

rootPath :: Tree -> Tree
rootPath t =
  snd $
  foldl'
    (\acc@(level, list) item@(level', it) ->
       if (level' < level || list == [])
         then (level', item : list)
         else acc)
    (0, [])
    t

getPaths :: Name -> Tree -> [Tree]
getPaths n t = do
  let trees = treesToItem n t
   in map (rootPath . reverse) trees
