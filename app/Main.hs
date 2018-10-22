{-# LANGUAGE OverloadedStrings #-}

module Main where

import DepTree (getPaths, Tree)
import Control.Monad
import qualified Data.Text as T

testTree :: Tree
testTree = [(0, "0", "0"), (1, "1A", "+- 1A"), (2, "2A", "|  +- 2A"), (3, "3x", "|  |  +- 3x"), (1, "1B", "+- 1B"), (2, "2B", "|  +- 2B"), (3, "3x", "|  |  +- 3x")]


main :: IO ()
main = do
  print $ getPaths "3x" testTree
  forM_ (concat $ getPaths "3x" testTree) (\(_, _, line) -> putStrLn $ T.unpack line)
