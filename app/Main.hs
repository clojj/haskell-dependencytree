{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Data.Text     (unpack)
import           DepTree       (Tree, getPaths)

testTree :: Tree
testTree = [(0, "0", "0"), (1, "1A", "+- 1A"), (2, "2A", "|  +- 2A"), (3, "3x", "|  |  +- 3x"), (1, "1B", "+- 1B"), (2, "2B", "|  +- 2B"), (3, "3x", "|  |  +- 3x")]

main :: IO ()
main = forM_ outputLines putStrLn
  where
    paths = concat $ getPaths "3x" testTree
    outputLines = [unpack line | (_, _, line) <- paths]
