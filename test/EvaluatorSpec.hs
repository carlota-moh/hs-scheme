module EvaluatorSpec
  ( spec
  ) where

import           Data       (LispVal (..))
import           Evaluator  (eval)
import           Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "eval" $ do
    it "evaluates a LispVal String into a LispVal String"
      $ eval (String "hello") `shouldBe` String "hello"
    it "evaluates a LispVal Number into a LispVal Number"
      $ eval (Number 123) `shouldBe` Number 123
    it "evaluates a LispVal Bool into a LispVal Bool"
      $ eval (Bool True) `shouldBe` Bool True
    it "evaluates a LispVal QuotedList into a LispVal with the content"
      $ eval (List [Atom "quote", String "value"]) `shouldBe` String "value"
    it "evaluates a LispVal List into a LispVal List"
      $ eval (List [Number 1, Number 2]) `shouldBe` List [Number 1, Number 2]
    it "evaluates a LispVal DottedList into a LispVal DottedList"
      $ eval (DottedList [Number 1, Number 2] (String "value"))
          `shouldBe` DottedList [Number 1, Number 2] (String "value")
    it "evaluates operations and returns the result as a LispVal Number"
      $ eval (List [Atom "+" , Number 1, Number 2]) `shouldBe` Number 3
