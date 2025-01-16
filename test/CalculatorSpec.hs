-- test/Tutorial/CalculatorSpec.hs
module CalculatorSpec (spec) where

import Test.Hspec
import Text.Parsec (parse)
import Tutorial.Calculator

spec :: Spec
spec = do
  describe "Calculator Parser" $ do
    it "parses simple number" $ do
      parse parseExpr "" "123" `shouldBe` Right (Number 123)
    
    it "parses addition" $ do
      parse parseExpr "" "1 + 2" `shouldBe` Right (Add (Number 1) (Number 2))
    
    it "parses multiplication with precedence" $ do
      parse parseExpr "" "1 + 2 * 3" `shouldBe` 
        Right (Add (Number 1) (Mul (Number 2) (Number 3)))
