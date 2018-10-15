{-# LANGUAGE OverloadedStrings #-}

module DepTree.DepTreeSpec
  ( main
  , spec
  ) where

import Test.Hspec
import Test.QuickCheck

import DepTree

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

testTree :: Tree
testTree = [(0, "0"), (1, "1A"), (2, "2A"), (3, "3x"), (1, "1B"), (2, "3x")]

depTxtTree :: Tree
depTxtTree =
  [ (0, "com.example:producer:jar:0.0.1-SNAPSHOT")
  , (3, "org.springframework.cloud:spring-cloud-stream:jar:2.0.0.RC3:compile")
  , (6, "org.springframework.boot:spring-boot-starter-validation:jar:2.0.2.RELEASE:compile")
  , (9, "org.apache.tomcat.embed:tomcat-embed-el:jar:8.5.31:compile")
  , (6, "org.springframework:spring-messaging:jar:5.0.6.RELEASE:compile")
  , (9, "org.springframework:spring-beans:jar:5.0.6.RELEASE:compile")
  ]

spec :: Spec
spec = do
  describe "file to Tree" $ do
    it "read file dep.txt and create Tree" $ do
      tree <- toTree "dep.txt"
      tree `shouldBe` depTxtTree
  describe "getPath" $ do
    it "all paths (from root) to node '2A'" $ do getPaths "2A" testTree `shouldBe` [["0", "1A", "2A"]]
    it "all paths (from root) to node '3x'" $ do
      getPaths "3x" testTree `shouldBe` [["0", "1B", "3x"], ["0", "1A", "2A", "3x"]]
    it "get path to node 'org.apache.tomcat.embed:tomcat-embed-el:jar:8.5.31:compile'" $ do
      getPaths "org.apache.tomcat.embed:tomcat-embed-el:jar:8.5.31:compile" depTxtTree `shouldBe`
        [ [ "com.example:producer:jar:0.0.1-SNAPSHOT"
          , "org.springframework.cloud:spring-cloud-stream:jar:2.0.0.RC3:compile"
          , "org.springframework.boot:spring-boot-starter-validation:jar:2.0.2.RELEASE:compile"
          , "org.apache.tomcat.embed:tomcat-embed-el:jar:8.5.31:compile"
          ]
        ]
    it "no paths" $ do getPaths "42" testTree `shouldBe` []