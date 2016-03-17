module Tests.LiteralParser
    ( testLiteralParser
    ) where

import Test.Hspec

import Control.Monad (forM_)
import Data.Char (chr)

import Language.JavaScript.Parser
import Language.JavaScript.Parser.Grammar5
import Language.JavaScript.Parser.Parser


testLiteralParser :: Spec
testLiteralParser = describe "Parse literals:" $ do
    it "null/true/false" $ do
        testLiteral "null"  `shouldBe` "Right (JSAstLiteral (JSLiteral 'null'))"
        testLiteral "false" `shouldBe` "Right (JSAstLiteral (JSLiteral 'false'))"
        testLiteral "true"  `shouldBe` "Right (JSAstLiteral (JSLiteral 'true'))"
    it "hex numbers" $ do
        testLiteral "0x1234fF"  `shouldBe` "Right (JSAstLiteral (JSHexInteger '0x1234fF'))"
        testLiteral "0X1234fF"  `shouldBe` "Right (JSAstLiteral (JSHexInteger '0X1234fF'))"
    it "decimal numbers" $ do
        testLiteral "1.0e4"     `shouldBe` "Right (JSAstLiteral (JSDecimal '1.0e4'))"
        testLiteral "2.3E6"     `shouldBe` "Right (JSAstLiteral (JSDecimal '2.3E6'))"
        testLiteral "4.5"       `shouldBe` "Right (JSAstLiteral (JSDecimal '4.5'))"
        testLiteral "0.7e8"     `shouldBe` "Right (JSAstLiteral (JSDecimal '0.7e8'))"
        testLiteral "0.7E8"     `shouldBe` "Right (JSAstLiteral (JSDecimal '0.7E8'))"
        testLiteral "10"        `shouldBe` "Right (JSAstLiteral (JSDecimal '10'))"
        testLiteral "0"         `shouldBe` "Right (JSAstLiteral (JSDecimal '0'))"
        testLiteral "0.03"      `shouldBe` "Right (JSAstLiteral (JSDecimal '0.03'))"
        testLiteral "0.7e+8"    `shouldBe` "Right (JSAstLiteral (JSDecimal '0.7e+8'))"
        testLiteral "0.7e-18"   `shouldBe` "Right (JSAstLiteral (JSDecimal '0.7e-18'))"
        testLiteral "1.0e+4"    `shouldBe` "Right (JSAstLiteral (JSDecimal '1.0e+4'))"
        testLiteral "1.0e-4"    `shouldBe` "Right (JSAstLiteral (JSDecimal '1.0e-4'))"
        testLiteral "1e18"      `shouldBe` "Right (JSAstLiteral (JSDecimal '1e18'))"
        testLiteral "1e+18"     `shouldBe` "Right (JSAstLiteral (JSDecimal '1e+18'))"
        testLiteral "1e-18"     `shouldBe` "Right (JSAstLiteral (JSDecimal '1e-18'))"
        testLiteral "1E-01"     `shouldBe` "Right (JSAstLiteral (JSDecimal '1E-01'))"
    it "octal numbers" $
        testLiteral "010"       `shouldBe` "Right (JSAstLiteral (JSOctal '010'))"
    it "strings" $ do
        testLiteral "'cat'"    `shouldBe` "Right (JSAstLiteral (JSStringLiteral 'cat'))"
        testLiteral "\"cat\""  `shouldBe` "Right (JSAstLiteral (JSStringLiteral \"cat\"))"
        testLiteral "\"hello\\nworld\"" `shouldBe` "Right (JSAstLiteral (JSStringLiteral \"hello\\nworld\"))"
        testLiteral "'hello\\nworld'"   `shouldBe` "Right (JSAstLiteral (JSStringLiteral 'hello\\nworld'))"

        forM_ (filter (/= '"') asciiTestString) $ \ ch -> do
            let str = "char " ++ [ch]
            testLiteral ("\"" ++ str ++ "\"")   `shouldBe` ("Right (JSAstLiteral (JSStringLiteral \"" ++ str ++ "\"))")
        forM_ (filter (/= '\'') asciiTestString) $ \ ch -> do
            let str = "char " ++ [ch]
            testLiteral ("'" ++ str ++ "'")     `shouldBe` ("Right (JSAstLiteral (JSStringLiteral '" ++ str ++ "'))")

        testLiteral "\"\r\n\"" `shouldBe` "Right (JSAstLiteral (JSStringLiteral \"\r\n\"))"


-- 8 bit ASCII, minus the backslash escape character.
asciiTestString :: String
asciiTestString = filter (/= '\\') $ map chr [0 .. 255]


testLiteral :: String -> String
testLiteral str = showStrippedMaybe (parseUsing parseLiteral str "src")
