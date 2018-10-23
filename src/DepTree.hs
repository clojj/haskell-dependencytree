module DepTree
  ( getPaths
  , fileToTree
  , Tree
  ) where

import           Data.List    (findIndices, foldl', map, reverse, take)
import           Data.Text    (Text (..), break, length, lines)
import           Data.Text.IO (readFile)
import           Prelude      hiding (break, length, lines, readFile)

import           Data.Char    (isAlpha)

type Name = Text

type TreeItem = (Int, Text, Text)

type Tree = [TreeItem]

type ResultItem = (Name, Int, Text)


findItem :: Name -> Tree -> [Int]
findItem name = findIndices (\(_, itemname, _) -> itemname == name)

treesToItem :: Name -> Tree -> [Tree]
treesToItem name tree =
  case findItem name tree of
    []      -> []
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
  content <- readFile file
  let ls = lines content
   in return $ map lineToTreeItem ls

lineToTreeItem :: Text -> TreeItem
lineToTreeItem line =
  let (pre, post) = break isAlpha line
   in (length pre, post, line)
