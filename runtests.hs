
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
      testCase "helloWorld"       caseHelloWorld  
    , testCase "LiteralNull"      caseLiteralNull  
    ]

srcHelloWorld = "Hello"
caseHelloWorld =  
  "Right (JSIdentifier \"Hello\")"
  @=? (show $ parseStmt srcHelloWorld "src")
  
caseLiteralNull =
  "Right (NullToken {token_span = SpanCoLinear {span_filename = \"src\", span_row = 1, span_start_column = 1, span_end_column = 4}})"
  @=? (show $ parseUsing parseLiteral "null" "src")

-- EOF
