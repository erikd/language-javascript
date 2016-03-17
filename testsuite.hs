
import Control.Monad (when)
import System.Exit
import Test.Hspec
import Test.Hspec.Runner


import Tests.Lexer
import Tests.LiteralParser
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
    testMinifyProg

