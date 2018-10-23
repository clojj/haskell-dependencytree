{-# LANGUAGE OverloadedStrings #-}

module DepTree.DepTreeSpec
  ( main
  , spec
  ) where

import           Test.Hspec
import           Test.QuickCheck

import           DepTree

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

testTree :: Tree
testTree = [(0, "0", "0"), (1, "1A", "+- 1A"), (2, "2A", "|  +- 2A"), (3, "2B", "|  |  +- "), (1, "1B", "+- 1B"), (2, "2B", "|  +- 2B")]

depTxtTree :: Tree
depTxtTree =
  [ (0, "com.example:producer:jar:0.0.1-SNAPSHOT", "com.example:producer:jar:0.0.1-SNAPSHOT")
  , (3, "org.springframework.cloud:spring-cloud-stream:jar:2.0.0.RC3:compile", "+- org.springframework.cloud:spring-cloud-stream:jar:2.0.0.RC3:compile")
  , (6, "org.springframework.boot:spring-boot-starter-validation:jar:2.0.2.RELEASE:compile", "|  +- org.springframework.boot:spring-boot-starter-validation:jar:2.0.2.RELEASE:compile")
  , (6, "A", "|  +- A")
  , (9, "org.apache.tomcat.embed:tomcat-embed-el:jar:8.5.31:compile", "|  |  +- org.apache.tomcat.embed:tomcat-embed-el:jar:8.5.31:compile")
  , (12, "C", "|  |  |  \\- C")
  , (6, "org.springframework:spring-messaging:jar:5.0.6.RELEASE:compile", "|  +- org.springframework:spring-messaging:jar:5.0.6.RELEASE:compile")
  , (6, "C", "|  +- C")
  , (9, "org.springframework:spring-beans:jar:5.0.6.RELEASE:compile", "      \\- org.springframework:spring-beans:jar:5.0.6.RELEASE:compile")
  ]

spec :: Spec
spec = do

  describe "fileToTree" $
    it "read file dep.txt and create Tree" $ do
      tree <- fileToTree "dep.txt"
      tree `shouldBe` depTxtTree

  describe "getPath" $ do
    it "all paths (from root) to node '2A'" $ getPaths "2A" testTree `shouldBe` [[("0", 0, "0"), ("1A", 1, "+- 1A"), ("2A", 2, "|  +- 2A")]]

    it "all paths (from root) to node '2B'" $ getPaths "2B" testTree `shouldBe` [[("0", 0, "0"), ("1B", 1, "+- 1B"), ("2B", 2, "|  +- 2B")], [("0", 0, "0"), ("1A", 1, "+- 1A"), ("2A", 2, "|  +- 2A"), ("2B", 3, "|  |  +- ")]]

    it "get path to node 'C'" $
      getPaths "C" depTxtTree `shouldBe`
      [ [ ("com.example:producer:jar:0.0.1-SNAPSHOT", 0, "com.example:producer:jar:0.0.1-SNAPSHOT")
        , ("org.springframework.cloud:spring-cloud-stream:jar:2.0.0.RC3:compile", 3, "+- org.springframework.cloud:spring-cloud-stream:jar:2.0.0.RC3:compile")
        , ("C", 6, "|  +- C")
        ]
      , [ ("com.example:producer:jar:0.0.1-SNAPSHOT", 0, "com.example:producer:jar:0.0.1-SNAPSHOT")
      , ("org.springframework.cloud:spring-cloud-stream:jar:2.0.0.RC3:compile", 3, "+- org.springframework.cloud:spring-cloud-stream:jar:2.0.0.RC3:compile")
        , ("A", 6, "|  +- A")
        , ("org.apache.tomcat.embed:tomcat-embed-el:jar:8.5.31:compile", 9, "|  |  +- org.apache.tomcat.embed:tomcat-embed-el:jar:8.5.31:compile")
        , ("C", 12, "|  |  |  \\- C")
        ]
      ]
    it "no paths" $ getPaths "42" testTree `shouldBe` []
