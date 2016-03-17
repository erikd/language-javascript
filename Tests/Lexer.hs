module Tests.Lexer
    ( testLexer
    ) where

import Test.Hspec

import Data.List (intercalate)

import Language.JavaScript.Parser.Lexer

testLexer :: Spec
testLexer = describe "Lexer:" $ do
    it "assignment" $ do
        testLex "x=1"        `shouldBe` "[IdentifierToken,SimpleAssignToken,DecimalToken]"
        testLex "x=1\ny=2"   `shouldBe` "[IdentifierToken,SimpleAssignToken,DecimalToken,WsToken,IdentifierToken,SimpleAssignToken,DecimalToken]"
    it "break/continue/return" $ do
        testLex "break\nx=1"     `shouldBe` "[BreakToken,WsToken,IdentifierToken,SimpleAssignToken,DecimalToken]"
        testLex "continue\nx=1"  `shouldBe` "[ContinueToken,WsToken,IdentifierToken,SimpleAssignToken,DecimalToken]"
        testLex "return\nx=1"    `shouldBe` "[ReturnToken,WsToken,IdentifierToken,SimpleAssignToken,DecimalToken]"


testLex :: String -> String
testLex str =
    either id stringify (alexTestTokeniser str)
  where
    stringify xs = "[" ++ intercalate "," (map (takeWhile (/= ' ') . show) xs) ++ "]"
