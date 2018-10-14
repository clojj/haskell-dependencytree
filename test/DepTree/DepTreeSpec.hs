module DepTree.DepTreeSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import DepTree

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

testTree :: Tree
testTree = [(0, "0"), (1, "1A"), (2, "2A"), (3, "3x"), (1, "1B"), (2, "3x")]

spec :: Spec
spec = do
  describe "getPath" $ do
    it "todo" $ do
      getPaths "3x" testTree `shouldBe`  [[(0,"0"),(1,"1B"),(2,"3x")],[(0,"0"),(1,"1A"),(2,"2A"),(3,"3x")]]
