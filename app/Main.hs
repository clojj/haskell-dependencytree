module Main where

import DepTree (getPaths, Tree)

testTree :: Tree
testTree = [(0, "0"), (1, "1A"), (2, "2A"), (3, "3x"), (1, "1B"), (2, "3x")]


main :: IO ()
main =  print $ getPaths "3x" testTree
