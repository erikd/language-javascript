
import Control.Monad (when)
import Data.List (intercalate)
import System.Exit
import Test.Hspec
import Test.Hspec.Runner

import Language.JavaScript.Parser
import Language.JavaScript.Parser.Grammar5
import Language.JavaScript.Parser.Lexer
import Language.JavaScript.Parser.Parser

import Tests.ExpressionParser
import Tests.Minify
import Tests.ProgramParser
import Tests.RoundTrip
import Tests.StatementParser


main :: IO ()
main = do
    summary <- hspecWithResult defaultConfig testAll
    when (summaryFailures summary == 0)
        exitSuccess
    exitFailure


testAll :: Spec
testAll = do
    testLexer
    testLiteralParser
    testExpressionParser
    testStatementParser
    testProgramParser
    testRoundTrip
    testMinifyExpr
    testMinifyStmt


testLexer :: Spec
testLexer = describe "Lexer:" $ do
    it "assignment" $ do
        testLex "x=1"        `shouldBe` "[IdentifierToken,SimpleAssignToken,DecimalToken]"
        testLex "x=1\ny=2"   `shouldBe` "[IdentifierToken,SimpleAssignToken,DecimalToken,WsToken,IdentifierToken,SimpleAssignToken,DecimalToken]"
    it "break/continure/return" $ do
        testLex "break\nx=1"     `shouldBe` "[BreakToken,WsToken,IdentifierToken,SimpleAssignToken,DecimalToken]"
        testLex "continue\nx=1"  `shouldBe` "[ContinueToken,WsToken,IdentifierToken,SimpleAssignToken,DecimalToken]"
        testLex "return\nx=1"    `shouldBe` "[ReturnToken,WsToken,IdentifierToken,SimpleAssignToken,DecimalToken]"


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
        testLiteral "\"hello\\nworld\"" `shouldBe` "Right (JSAstLiteral (JSStringLiteralD 'hello\\nworld'))"
        testLiteral "'hello\\nworld'"   `shouldBe` "Right (JSAstLiteral (JSStringLiteralS 'hello\\nworld'))"


testLex :: String -> String
testLex str =
    either id stringify (alexTestTokeniser str)
  where
    stringify xs = "[" ++ intercalate "," (map (takeWhile (/= ' ') . show) xs) ++ "]"


testLiteral :: String -> String
testLiteral str = showStrippedMaybe (parseUsing parseLiteral str "src")
