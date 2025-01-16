-- test/Tutorial/BasicParsersSpec.hs
module Tutorial.BasicParsersSpec (spec) where

import Test.Hspec
import Text.Parsec (parse)
import Tutorial.BasicParsers

spec :: Spec
spec = do
  describe "Basic Parsers" $ do
    describe "parseDigit" $ do
      it "parses a single digit" $ do
        parse parseDigit "" "5" `shouldBe` Right '5'
      
      it "fails on non-digit" $ do
        parse parseDigit "" "a" `shouldBe` Left "digit"

    describe "parseNumber" $ do
      it "parses multiple digits" $ do
        parse parseNumber "" "123" `shouldBe` Right "123"
      
      it "fails on non-number" $ do
        parse parseNumber "" "abc" `shouldBe` Left "digit"

    describe "parseWord" $ do
      it "parses a single word" $ do
        parse parseWord "" "hello" `shouldBe` Right "hello"
      
      it "stops at space" $ do
        parse parseWord "" "hello world" `shouldBe` Right "hello"

    describe "parseSentence" $ do
      it "parses multiple words" $ do
        parse parseSentence "" "hello world" `shouldBe` Right ["hello", "world"]
      
      it "handles multiple spaces" $ do
        parse parseSentence "" "hello   world" `shouldBe` Right ["hello", "world"]

    describe "parseCSV" $ do
      it "parses single line CSV" $ do
        parse parseCSV "" "hello,world" `shouldBe` Right [["hello", "world"]]
      
      it "parses multiple lines" $ do
        parse parseCSV "" "a,b,c\nd,e,f" `shouldBe` Right [["a","b","c"], ["d","e","f"]]
