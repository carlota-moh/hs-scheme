module ParserSpec
  ( spec
  ) where

import           Data              (LispVal (..))
import           Parser            (parseAtom, parseList, parseNumber,
                                    parseString)
import           Test.Hspec        (Spec, describe, it)
import           Test.Hspec.Parsec (shouldParse)
import           Text.Parsec       (parse)

spec :: Spec
spec = do
  describe "parseString" $ do
    let testParseString = parse parseString ""
    it "parses a string into a LispVal String"
      $ testParseString "\"hello\"" `shouldParse` String "hello"
  describe "parseAtom" $ do
    let testParseAtom = parse parseAtom ""
    it "parses a string starting with letter into a LispVal Atom"
      $ testParseAtom "atom!" `shouldParse` Atom "atom!"
    it "parses a string starting with symbol into a LispVal Atom"
      $ testParseAtom "#tom!" `shouldParse` Atom "#tom!"
    it "parses a literal bool strings into LispVal Bool" $ do
      testParseAtom "#t" `shouldParse` Bool True
      testParseAtom "#f" `shouldParse` Bool False
  describe "parseNumber" $ do
    let testParseNumber = parse parseNumber ""
    it "parses a string with a number into a LispVal Number"
      $ testParseNumber "1" `shouldParse` Number 1
  describe "parseList" $ do
    let testParseList = parse parseList ""
    it "parses a values separated by spaces into a LispVal List"
      $ testParseList "1 #tom! \"hello\"" `shouldParse` List [Number 1, Atom "#tom!", String "hello"]
