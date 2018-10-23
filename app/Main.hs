{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Data.Text     (unlines, unpack)
import           DepTree       (Tree, getPaths)
import           Prelude       hiding (unlines)

testTree :: Tree
testTree =
  [ (0, "0", "0")
  , (1, "1A", "+- 1A")
  , (2, "2A", "|  +- 2A")
  , (3, "3x", "|  |  +- 3x")
  , (1, "1B", "+- 1B")
  , (2, "2B", "|  +- 2B")
  , (3, "3x", "|  |  +- 3x")
  ]

main :: IO ()
main = forM_ outputPaths (putStrLn . unpack)
  where
    paths = getPaths "3x" testTree
    outputPaths = outputPath <$> paths
    outputPath resultItems = unlines [line | (_, _, line) <- resultItems]
