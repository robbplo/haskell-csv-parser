module Main (main) where

import CsvParser (Csv (..), fromString)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "CsvParser.fromString" $ do
    it "parses empty string" $ do
      fromString False "" `shouldBe` Right (Csv Nothing [])
      fromString True "" `shouldBe` Right (Csv Nothing [])
    it "parses single line with single cell" $ do
      fromString False "hello" `shouldBe` Right (Csv Nothing [["hello"]])
      fromString True "hello" `shouldBe` Right (Csv (Just ["hello"]) [])
    it "parses single line with multiple cells" $ do
      fromString False "hi,how,are,you" `shouldBe` Right (Csv Nothing [["hi", "how", "are", "you"]])
      fromString True "hi,how,are,you" `shouldBe` Right (Csv (Just ["hi", "how", "are", "you"]) [])
    it "parses multiple lines with multiple cells" $ do
      fromString False "hi,how,are,you\ni,am,well,thanks"
        `shouldBe` Right
          ( Csv
              Nothing
              [ ["hi", "how", "are", "you"],
                ["i", "am", "well", "thanks"]
              ]
          )
      fromString True "hi,how,are,you\ni,am,well,thanks"
        `shouldBe` Right
          ( Csv
              (Just ["hi", "how", "are", "you"])
              [["i", "am", "well", "thanks"]]
          )
    it "parses quoted cells with commas" $ do
      fromString False "\"it,even,does,quoted,stuff!\",and,here,is,more"
        `shouldBe` Right
          ( Csv
              Nothing
              [ [ "\"it,even,does,quoted,stuff!\"",
                  "and",
                  "here",
                  "is",
                  "more"
                ]
              ]
          )
