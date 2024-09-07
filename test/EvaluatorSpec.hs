module EvaluatorSpec
  ( spec
  ) where

import           Data       (LispVal (..))
import           Evaluator  (eval)
import           Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "eval" $ do
    describe "simple values" $ do
      it "evaluates a LispVal String into a LispVal String"
        $ eval (String "hello") `shouldBe` Right (String "hello")
      it "evaluates a LispVal Number into a LispVal Number"
        $ eval (Number 123) `shouldBe` Right (Number 123)
      it "evaluates a LispVal Bool into a LispVal Bool"
        $ eval (Bool True) `shouldBe` Right (Bool True)
      it "evaluates a LispVal QuotedList into a LispVal with the content"
        $ eval (List [Atom "quote", String "value"])
            `shouldBe` Right (String "value")
      it "evaluates a LispVal List into a LispVal List"
        $ eval (List [Number 1, Number 2])
            `shouldBe` Right (List [Number 1, Number 2])
      it "evaluates a LispVal DottedList into a LispVal DottedList"
        $ eval (DottedList [Number 1, Number 2] (String "value"))
            `shouldBe` Right (DottedList [Number 1, Number 2] (String "value"))
      it "evaluates operations and returns the result as a LispVal Number"
        $ eval (List [Atom "+", Number 1, Number 2]) `shouldBe` Right (Number 3)
    describe "if-else" $ do
      it "returns consequence when predicate is true"
        $ eval
            (List
               [ Atom "if"
               , List [Atom "<", Number 2, Number 3]
               , String "smaller"
               , String "bigger"
               ])
            `shouldBe` Right (String "smaller")
      it "returns alternative when predicate is false"
        $ eval
            (List
               [ Atom "if"
               , List [Atom ">", Number 2, Number 3]
               , String "smaller"
               , String "bigger"
               ])
            `shouldBe` Right (String "bigger")
