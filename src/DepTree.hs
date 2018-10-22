
module DepTree
  ( getPaths
  , fileToTree
  , Tree
  ) where

import Data.List (findIndex, findIndices, foldl, foldl', foldr, map, reverse, take)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Char (isAlpha)

type Name = T.Text

type TreeItem = (Int, T.Text, T.Text)

type Tree = [TreeItem]

type ResultItem = (Name, Int, T.Text)


findItem :: Name -> Tree -> [Int]
findItem name = findIndices (\(_, itemname, _) -> itemname == name)

treesToItem :: Name -> Tree -> [Tree]
treesToItem name tree =
  case findItem name tree of
    [] -> []
    indexes -> foldl' (\list n -> take (n + 1) tree : list) [] indexes

rootPath :: Tree -> [ResultItem]
rootPath tree = snd $ foldl' fun (0, []) tree

fun :: (Int, [ResultItem]) -> TreeItem -> (Int, [ResultItem])
fun acc@(level, names) item@(itemLevel, itemName, itemLine)
  | itemLevel < level || null names = (itemLevel, (itemName, itemLevel, itemLine) : names)
  | otherwise = acc

getPaths :: Name -> Tree -> [[ResultItem]]
getPaths name tree =
  let trees = treesToItem name tree
   in map (rootPath . reverse) trees

fileToTree :: FilePath -> IO Tree
fileToTree file = do
  content <- TIO.readFile file
  let lines = T.lines content
   in return $ map lineToTreeItem lines

lineToTreeItem :: T.Text -> TreeItem
lineToTreeItem line =
  let (pre, post) = T.break isAlpha line
   in (T.length pre, post, line)