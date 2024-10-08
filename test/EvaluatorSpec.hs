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
      it "evaluates boolean operations"
        $ eval (List [Atom "<", Number 1, Number 2])
            `shouldBe` Right (Bool True)
      describe "car primitive" $ do
        it "returns first element of a List"
          $ eval (List [Atom "car", List [Number 2, Number 3, Number 4]])
              `shouldBe` Right (Number 2)
        it "returns first element of List inside a DottedList"
          $ eval
              (List
                 [Atom "car", DottedList [Number 2, Number 3] (String "value")])
              `shouldBe` Right (Number 2)
      describe "cdr primitive" $ do
        it "returns tail of a List"
          $ eval (List [Atom "cdr", List [Number 2, Number 3, Number 4]])
              `shouldBe` Right (List [Number 3, Number 4])
        it "returns tail of each element inside a DottedList"
          $ eval
              (List
                 [Atom "cdr", DottedList [Number 2, Number 3] (String "value")])
              `shouldBe` Right (DottedList [Number 3] (String "value"))
        it
          "returns tail element if List inside a DottedList only has one element"
          $ eval (List [Atom "cdr", DottedList [Number 2] (String "value")])
              `shouldBe` Right (String "value")
      describe "cons primitive" $ do
        it "concatenates single element with empty list"
          $ eval (List [Atom "cons", Number 1, List []])
              `shouldBe` Right (List [Number 1])
        it "concatenates single element with non-emtpy list"
          $ eval (List [Atom "cons", Number 1, List [Number 2]])
              `shouldBe` Right (List [Number 1, Number 2])
        it "concatenates element to List inside DottedList"
          $ eval
              (List
                 [Atom "cons", Number 1, DottedList [Number 2] (String "value")])
              `shouldBe` Right
                           (DottedList [Number 1, Number 2] (String "value"))
        it "returns DottedList when applied to two non-list elements"
          $ eval (List [Atom "cons", Number 1, Number 2])
              `shouldBe` Right (DottedList [Number 1] (Number 2))
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
