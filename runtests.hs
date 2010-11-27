
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)


import Language.JavaScript.Parser.Parser
import Language.JavaScript.Parser.Grammar

main :: IO ()
main = defaultMain [testSuite]

testSuite :: Test
testSuite = testGroup "Parser"
    [ 
      testCase "helloWorld"        caseHelloWorld  
    , testCase "LiteralNull"       (testLiteral "null"     "Right (JSLiteral \"null\")")
    , testCase "LiteralFalse"      (testLiteral "false"    "Right (JSLiteral \"false\")")
    , testCase "LiteralTrue"       (testLiteral "true"     "Right (JSLiteral \"true\")")
    , testCase "LiteralHexInteger" (testLiteral "0x1234fF" "Right (JSHexInteger \"0x1234fF\")")
    ]

srcHelloWorld = "Hello"
caseHelloWorld =  
  "Right (JSIdentifier \"Hello\")"
  @=? (show $ parseStmt srcHelloWorld "src")
  

-- ---------------------------------------------------------------------
-- Test utilities

testLiteral literal expected = expected @=? (show $ parseUsing parseLiteral literal "src")

-- EOF
