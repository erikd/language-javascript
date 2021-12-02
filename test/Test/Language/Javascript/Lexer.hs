module Test.Language.Javascript.Lexer
    ( testLexer
    ) where

import Test.Hspec

import Data.List (intercalate)

import Language.JavaScript.Parser.Lexer


testLexer :: Spec
testLexer = describe "Lexer:" $ do
    describe "with Alex rules" $ do
        it "comments" $ do
            alexTestLex "// 𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡 "    `shouldBe` "[CommentToken]"
            alexTestLex "/* 𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡 */"  `shouldBe` "[CommentToken]"

        it "return mixed with comments" $ do
            alexTestLex "return 1"  `shouldBe` "[ReturnToken,WsToken,DecimalToken 1]"
            alexTestLex "return \n 1"  `shouldBe` "[ReturnToken,WsToken,DecimalToken 1]"
            alexTestLex "return //hello"  `shouldBe` "[ReturnToken,WsToken,CommentToken]"
            alexTestLex "return /*hello*/"  `shouldBe` "[ReturnToken,WsToken,CommentToken]"
            alexTestLex "return //hello\n 1"  `shouldBe` "[ReturnToken,WsToken,CommentToken,WsToken,DecimalToken 1]"
            alexTestLex "return /*hello*/\n 1"  `shouldBe` "[ReturnToken,WsToken,CommentToken,WsToken,DecimalToken 1]"
            alexTestLex "return /*hello 1*/\n"  `shouldBe` "[ReturnToken,WsToken,CommentToken,WsToken]"

        it "numbers" $ do
            alexTestLex "123"       `shouldBe` "[DecimalToken 123]"
            alexTestLex "037"       `shouldBe` "[OctalToken 037]"
            alexTestLex "0xab"      `shouldBe` "[HexIntegerToken 0xab]"
            alexTestLex "0xCD"      `shouldBe` "[HexIntegerToken 0xCD]"

        it "invalid numbers" $ do
            alexTestLex "089"       `shouldBe` "[DecimalToken 0,DecimalToken 89]"
            alexTestLex "0xGh"      `shouldBe` "[DecimalToken 0,IdentifierToken 'xGx']"

        it "string" $ do
            alexTestLex "'cat'"     `shouldBe` "[StringToken 'cat']"
            alexTestLex "\"dog\""   `shouldBe` "[StringToken \"dog\"]"

        it "strings with escape chars" $ do
            alexTestLex "'\t'"      `shouldBe` "[StringToken '\t']"
            alexTestLex "'\\n'"     `shouldBe` "[StringToken '\\n']"
            alexTestLex "'\\\\n'"   `shouldBe` "[StringToken '\\\\n']"
            alexTestLex "'\\\\'"    `shouldBe` "[StringToken '\\\\']"
            alexTestLex "'\\0'"     `shouldBe` "[StringToken '\\0']"
            alexTestLex "'\\12'"    `shouldBe` "[StringToken '\\12']"
            alexTestLex "'\\s'"      `shouldBe` "[StringToken '\\s']"
            alexTestLex "'\\-'"      `shouldBe` "[StringToken '\\-']"

        it "strings with non-escaped chars" $
            alexTestLex "'\\/'"     `shouldBe` "[StringToken '\\/']"

        it "strings with escaped quotes" $ do
            alexTestLex "'\"'"      `shouldBe` "[StringToken '\"']"
            alexTestLex "\"\\\"\""  `shouldBe` "[StringToken \"\\\\\"\"]"
            alexTestLex "'\\\''"    `shouldBe` "[StringToken '\\\\'']"
            alexTestLex "'\"'"      `shouldBe` "[StringToken '\"']"
            alexTestLex "\"\\'\""      `shouldBe` "[StringToken \"\\'\"]"

        it "spread token" $ do
            alexTestLex "...a" `shouldBe` "[SpreadToken,IdentifierToken 'a']"

        it "assignment" $ do
            alexTestLex "x=1"       `shouldBe` "[IdentifierToken 'x',SimpleAssignToken,DecimalToken 1]"
            alexTestLex "x=1\ny=2"  `shouldBe` "[IdentifierToken 'x',SimpleAssignToken,DecimalToken 1,WsToken,IdentifierToken 'y',SimpleAssignToken,DecimalToken 2]"

        it "break/continue/return" $ do
            alexTestLex "break\nx=1"     `shouldBe` "[BreakToken,WsToken,IdentifierToken 'x',SimpleAssignToken,DecimalToken 1]"
            alexTestLex "continue\nx=1"  `shouldBe` "[ContinueToken,WsToken,IdentifierToken 'x',SimpleAssignToken,DecimalToken 1]"
            alexTestLex "return\nx=1"    `shouldBe` "[ReturnToken,WsToken,IdentifierToken 'x',SimpleAssignToken,DecimalToken 1]"

        it "var/let" $ do
            alexTestLex "var\n"     `shouldBe` "[VarToken,WsToken]"
            alexTestLex "let\n"     `shouldBe` "[LetToken,WsToken]"

        it "in/of" $ do
            alexTestLex "in\n"     `shouldBe` "[InToken,WsToken]"
            alexTestLex "of\n"     `shouldBe` "[OfToken,WsToken]"

        it "function" $ do
            alexTestLex "async function\n"     `shouldBe` "[AsyncToken,WsToken,FunctionToken,WsToken]"


    describe "with Happy rules" $ do
        it "comments" $ do
            happyTestLex "// 𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡 "    `shouldBe` "[]"
            happyTestLex "/* 𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡 */"  `shouldBe` "[]"
            happyTestLex "/* 𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡 */ // foo"  `shouldBe` "[]"

        it "return that doesn't produce autosemi" $ do
            happyTestLex "return 1"  `shouldBe` "[ReturnToken,DecimalToken 1]"
            happyTestLex "return //hello"  `shouldBe` "[ReturnToken]"
            happyTestLex "return/*hello*/1"  `shouldBe` "[ReturnToken,DecimalToken 1]"

        it "return mixed with newlines that produce autosemi" $ do
            happyTestLex "return \n 1"  `shouldBe` "[ReturnToken,AutoSemiToken,DecimalToken 1]"

        it "return mixed with comments that produce autosemi and trailing expressions" $ do
            happyTestLex "return /*hello \n */"  `shouldBe` "[ReturnToken,AutoSemiToken]"
            happyTestLex "return /*hello \n 1*/"  `shouldBe` "[ReturnToken,AutoSemiToken]"
            happyTestLex "return //hello\n"  `shouldBe` "[ReturnToken,AutoSemiToken]"

        it "return mixed with comments that produce autosemi but no trailing expressions" $ do
            happyTestLex "return //hello\n 1"  `shouldBe` "[ReturnToken,AutoSemiToken,DecimalToken 1]"
            happyTestLex "return /*hello*/\n 1"  `shouldBe` "[ReturnToken,AutoSemiToken,DecimalToken 1]"
            happyTestLex "return//hello\n1"  `shouldBe` "[ReturnToken,AutoSemiToken,DecimalToken 1]"

        it "numbers" $ do
            happyTestLex "123"       `shouldBe` "[DecimalToken 123]"
            happyTestLex "037"       `shouldBe` "[OctalToken 037]"
            happyTestLex "0xab"      `shouldBe` "[HexIntegerToken 0xab]"
            happyTestLex "0xCD"      `shouldBe` "[HexIntegerToken 0xCD]"

        it "invalid numbers" $ do
            happyTestLex "089"       `shouldBe` "[DecimalToken 0,DecimalToken 89]"
            happyTestLex "0xGh"      `shouldBe` "[DecimalToken 0,IdentifierToken 'xGx']"

        it "string" $ do
            happyTestLex "'cat'"     `shouldBe` "[StringToken 'cat']"
            happyTestLex "\"dog\""   `shouldBe` "[StringToken \"dog\"]"

        it "strings with escape chars" $ do
            happyTestLex "'\t'"      `shouldBe` "[StringToken '\t']"
            happyTestLex "'\\n'"     `shouldBe` "[StringToken '\\n']"
            happyTestLex "'\\\\n'"   `shouldBe` "[StringToken '\\\\n']"
            happyTestLex "'\\\\'"    `shouldBe` "[StringToken '\\\\']"
            happyTestLex "'\\0'"     `shouldBe` "[StringToken '\\0']"
            happyTestLex "'\\12'"    `shouldBe` "[StringToken '\\12']"
            happyTestLex "'\\s'"      `shouldBe` "[StringToken '\\s']"
            happyTestLex "'\\-'"      `shouldBe` "[StringToken '\\-']"

        it "strings with non-escaped chars" $
            happyTestLex "'\\/'"     `shouldBe` "[StringToken '\\/']"

        it "strings with escaped quotes" $ do
            happyTestLex "'\"'"      `shouldBe` "[StringToken '\"']"
            happyTestLex "\"\\\"\""  `shouldBe` "[StringToken \"\\\\\"\"]"
            happyTestLex "'\\\''"    `shouldBe` "[StringToken '\\\\'']"
            happyTestLex "'\"'"      `shouldBe` "[StringToken '\"']"
            happyTestLex "\"\\'\""      `shouldBe` "[StringToken \"\\'\"]"

        it "spread token" $ do
            happyTestLex "...a" `shouldBe` "[SpreadToken,IdentifierToken 'a']"

        it "assignment" $ do
            happyTestLex "x=1"       `shouldBe` "[IdentifierToken 'x',SimpleAssignToken,DecimalToken 1]"
            happyTestLex "x=1\ny=2"  `shouldBe` "[IdentifierToken 'x',SimpleAssignToken,DecimalToken 1,IdentifierToken 'y',SimpleAssignToken,DecimalToken 2]"

        it "break/continue/return" $ do
            happyTestLex "break\nx=1"     `shouldBe` "[BreakToken,AutoSemiToken,IdentifierToken 'x',SimpleAssignToken,DecimalToken 1]"
            happyTestLex "continue\nx=1"  `shouldBe` "[ContinueToken,AutoSemiToken,IdentifierToken 'x',SimpleAssignToken,DecimalToken 1]"
            happyTestLex "return\nx=1"    `shouldBe` "[ReturnToken,AutoSemiToken,IdentifierToken 'x',SimpleAssignToken,DecimalToken 1]"

        it "var/let" $ do
            happyTestLex "var\n"     `shouldBe` "[VarToken]"
            happyTestLex "let\n"     `shouldBe` "[LetToken]"

        it "in/of" $ do
            happyTestLex "in\n"     `shouldBe` "[InToken]"
            happyTestLex "of\n"     `shouldBe` "[OfToken]"

        it "function" $ do
            happyTestLex "async function\n"     `shouldBe` "[AsyncToken,FunctionToken]"



alexTestLex :: String -> String
alexTestLex = genericTestLex alexTestTokeniser

happyTestLex :: String -> String
happyTestLex = genericTestLex happyTestTokeniser

genericTestLex :: (String -> Either String [Token]) -> String -> String
genericTestLex lexer str =
    either id stringify $ lexer str
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
