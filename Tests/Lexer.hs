module Tests.Lexer
    ( testLexer
    ) where

import Test.Hspec

import Data.List (intercalate)

import Language.JavaScript.Parser.Lexer


testLexer :: Spec
testLexer = describe "Lexer:" $ do
    it "string" $ do
        testLex "'cat'"     `shouldBe` "[StringToken 'cat']"
        testLex "\"dog\""   `shouldBe` "[StringToken \"dog\"]"

    it "strings with escape chars" $ do
        testLex "'\t'"      `shouldBe` "[StringToken '\t']"
        testLex "'\\n'"     `shouldBe` "[StringToken '\\n']"
        testLex "'\\\\n'"   `shouldBe` "[StringToken '\\\\n']"
        testLex "'\\\\'"    `shouldBe` "[StringToken '\\\\']"
        testLex "'\\0'"     `shouldBe` "[StringToken '\\0']"
        testLex "'\\12'"    `shouldBe` "[StringToken '\\12']"

    it "strings with escaped quotes" $ do
        testLex "'\"'"      `shouldBe` "[StringToken '\"']"
        testLex "\"\\\"\""  `shouldBe` "[StringToken \"\\\\\"\"]"
        testLex "'\\\''"    `shouldBe` "[StringToken '\\\\'']"

    it "assignment" $ do
        testLex "x=1"       `shouldBe` "[IdentifierToken 'x',SimpleAssignToken,DecimalToken]"
        testLex "x=1\ny=2"  `shouldBe` "[IdentifierToken 'x',SimpleAssignToken,DecimalToken,WsToken,IdentifierToken 'y',SimpleAssignToken,DecimalToken]"

    it "break/continue/return" $ do
        testLex "break\nx=1"     `shouldBe` "[BreakToken,WsToken,IdentifierToken 'x',SimpleAssignToken,DecimalToken]"
        testLex "continue\nx=1"  `shouldBe` "[ContinueToken,WsToken,IdentifierToken 'x',SimpleAssignToken,DecimalToken]"
        testLex "return\nx=1"    `shouldBe` "[ReturnToken,WsToken,IdentifierToken 'x',SimpleAssignToken,DecimalToken]"


testLex :: String -> String
testLex str =
    either id stringify $ alexTestTokeniser str
  where
    stringify xs = "[" ++ intercalate "," (map showToken xs) ++ "]"

    showToken :: Token -> String
    showToken (StringToken _ lit _) = "StringToken " ++ stringEscape lit
    showToken (IdentifierToken _ lit _) = "IdentifierToken '" ++ stringEscape lit ++ "'"
    showToken token = takeWhile (/= ' ') $ show token

    stringEscape [] = []
    stringEscape (term:rest) =
        let escapeTerm [] = []
            escapeTerm [_] = [term]
            escapeTerm (x:xs)
                | term == x = "\\" ++ x : escapeTerm xs
                | otherwise = x : escapeTerm xs
        in term : escapeTerm rest
