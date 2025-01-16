-- test/Tutorial/JSONSpec.hs
module JSONSpec (spec) where

import Test.Hspec
import Text.Parsec (parse)
import Tutorial.JSON
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "JSON Parser" $ do
    it "parses null" $ do
      parse parseJSON "" "null" `shouldBe` Right JNull
    
    it "parses boolean" $ do
      parse parseJSON "" "true" `shouldBe` Right (JBool True)
      parse parseJSON "" "false" `shouldBe` Right (JBool False)
    
    it "parses number" $ do
      parse parseJSON "" "123.45" `shouldBe` Right (JNumber 123.45)
    
    it "parses string" $ do
      parse parseJSON "" "\"hello\"" `shouldBe` Right (JString "hello")
    
    it "parses array" $ do
      parse parseJSON "" "[1, 2, 3]" `shouldBe` 
        Right (JArray [JNumber 1, JNumber 2, JNumber 3])
    
    it "parses object" $ do
      parse parseJSON "" "{\"name\": \"John\", \"age\": 30}" `shouldBe`
        Right (JObject $ Map.fromList [
          ("name", JString "John"),
          ("age", JNumber 30)
        ])
