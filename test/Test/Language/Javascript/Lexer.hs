module Test.Language.Javascript.Lexer
    ( testLexer
    ) where

import Test.Hspec

import Data.List (intercalate)

import Language.JavaScript.Parser.Lexer


testLexer :: Spec
testLexer = describe "Lexer:" $ do
    it "comments" $ do
        testLex "// 𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡 "    `shouldBe` "[CommentToken]"
        testLex "/* 𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡 */"  `shouldBe` "[CommentToken]"

    it "numbers" $ do
        testLex "123"       `shouldBe` "[DecimalToken 123]"
        testLex "037"       `shouldBe` "[OctalToken 037]"
        testLex "0xab"      `shouldBe` "[HexIntegerToken 0xab]"
        testLex "0xCD"      `shouldBe` "[HexIntegerToken 0xCD]"

    it "invalid numbers" $ do
        testLex "089"       `shouldBe` "[DecimalToken 0,DecimalToken 89]"
        testLex "0xGh"      `shouldBe` "[DecimalToken 0,IdentifierToken 'xGx']"

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
        testLex "'\\s'"      `shouldBe` "[StringToken '\\s']"
        testLex "'\\-'"      `shouldBe` "[StringToken '\\-']"

    it "strings with non-escaped chars" $
        testLex "'\\/'"     `shouldBe` "[StringToken '\\/']"

    it "strings with escaped quotes" $ do
        testLex "'\"'"      `shouldBe` "[StringToken '\"']"
        testLex "\"\\\"\""  `shouldBe` "[StringToken \"\\\\\"\"]"
        testLex "'\\\''"    `shouldBe` "[StringToken '\\\\'']"
        testLex "'\"'"      `shouldBe` "[StringToken '\"']"
        testLex "\"\\'\""      `shouldBe` "[StringToken \"\\'\"]"

    it "assignment" $ do
        testLex "x=1"       `shouldBe` "[IdentifierToken 'x',SimpleAssignToken,DecimalToken 1]"
        testLex "x=1\ny=2"  `shouldBe` "[IdentifierToken 'x',SimpleAssignToken,DecimalToken 1,WsToken,IdentifierToken 'y',SimpleAssignToken,DecimalToken 2]"

    it "break/continue/return" $ do
        testLex "break\nx=1"     `shouldBe` "[BreakToken,WsToken,IdentifierToken 'x',SimpleAssignToken,DecimalToken 1]"
        testLex "continue\nx=1"  `shouldBe` "[ContinueToken,WsToken,IdentifierToken 'x',SimpleAssignToken,DecimalToken 1]"
        testLex "return\nx=1"    `shouldBe` "[ReturnToken,WsToken,IdentifierToken 'x',SimpleAssignToken,DecimalToken 1]"


testLex :: String -> String
testLex str =
    either id stringify $ alexTestTokeniser str
  where
    stringify xs = "[" ++ intercalate "," (map showToken xs) ++ "]"

    showToken :: Token -> String
    showToken (StringToken _ lit _) = "StringToken " ++ stringEscape lit
    showToken (IdentifierToken _ lit _) = "IdentifierToken '" ++ stringEscape lit ++ "'"
    showToken (DecimalToken _ lit _) = "DecimalToken " ++ lit
    showToken (OctalToken _ lit _) = "OctalToken " ++ lit
    showToken (HexIntegerToken _ lit _) = "HexIntegerToken " ++ lit
    showToken token = takeWhile (/= ' ') $ show token

    stringEscape [] = []
    stringEscape (term:rest) =
        let escapeTerm [] = []
            escapeTerm [_] = [term]
            escapeTerm (x:xs)
                | term == x = "\\" ++ x : escapeTerm xs
                | otherwise = x : escapeTerm xs
        in term : escapeTerm rest
