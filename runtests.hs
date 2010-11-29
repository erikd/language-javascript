
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
    , testCase "LiteralDecimal1"   (testLiteral "1.0e4"    "Right (JSDecimal \"1.0e4\")")
    , testCase "LiteralDecimal2"   (testLiteral "2.3E6"    "Right (JSDecimal \"2.3E6\")")
    , testCase "LiteralDecimal3"   (testLiteral "4.5"      "Right (JSDecimal \"4.5\")")
    , testCase "LiteralDecimal3"   (testLiteral "0.7e8"    "Right (JSDecimal \"0.7e8\")")
    , testCase "LiteralDecimal4"   (testLiteral "0.7E8"    "Right (JSDecimal \"0.7E8\")")
    , testCase "LiteralDecimal5"   (testLiteral "10"       "Right (JSDecimal \"10\")")
    , testCase "LiteralDecimal6"   (testLiteral "0"        "Right (JSDecimal \"0\")")
    , testCase "LiteralDecimaly"   (testLiteral "0.03"     "Right (JSDecimal \"0.03\")")
      
    , testCase "LiteralString1"    (testLiteral "\"hello\\nworld\"" "Right (JSStringLiteral '\"' \"hello\\\\nworld\")")  
    , testCase "LiteralString2"    (testLiteral "'hello\\nworld'"  "Right (JSStringLiteral '\\'' \"hello\\\\nworld\")")
      
    , testCase "LiteralThis"       (testPE "this"  "Right (JSLiteral \"this\")")
      
    , testCase "LiteralRegex1"     (testPE "/blah/"  "Right (JSRegEx \"/blah/\")")
    , testCase "LiteralRegex2"     (testPE "/$/g"    "Right (JSRegEx \"/$/g\")")
      
    , testCase "Identifier1"       (testPE "_$"      "Right (JSIdentifier \"_$\")")
    , testCase "Identifier2"       (testPE "this_"   "Right (JSIdentifier \"this_\")")
      
    , testCase "ArrayLiteral1"     (testPE "[]"      "Right (JSArrayLiteral [])")
    , testCase "ArrayLiteral2"     (testPE "[,]"     "Right (JSArrayLiteral [JSElision []])")
    , testCase "ArrayLiteral3"     (testPE "[,,]"    "Right (JSArrayLiteral [JSElision [],JSElision []])")
    , testCase "ArrayLiteral4"     (testPE "[,,x]"   "Right (JSArrayLiteral [JSElementList [JSElision [],JSElision [],JSIdentifier \"x\"]])")
    , testCase "ArrayLiteral5"     (testPE "[,,x]"   "Right (JSArrayLiteral [JSElementList [JSElision [],JSElision [],JSIdentifier \"x\"]])")
    , testCase "ArrayLiteral6"     (testPE "[,x,,x]" "Right (JSArrayLiteral [JSElementList [JSElementList [JSElision [],JSIdentifier \"x\"],JSElision [],JSIdentifier \"x\"]])")
    , testCase "ArrayLiteral7"     (testPE "[x]"     "Right (JSArrayLiteral [JSIdentifier \"x\"])")
      
    , testCase "ObjectLiteral2"    (testPE "{x:1}"     "Right (JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"x\") [JSDecimal \"1\"]])")
    , testCase "ObjectLiteral3"    (testPE "{x:1,y:2}"     "Right (JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"x\") [JSDecimal \"1\"],JSPropertyNameandValue (JSIdentifier \"y\") [JSDecimal \"2\"]])")
      
    , testCase "ExpressionParen"   (testPE "(56)"     "Right (JSExpressionParen (JSExpression [JSDecimal \"56\"]))")
      
    , testCase "Statement1"        (testStmt "x"        "Right (JSExpression [JSIdentifier \"x\"])")
    , testCase "Statement2"        (testStmt "null"     "Right (JSExpression [JSLiteral \"null\"])")
    , testCase "Statement3"        (testStmt "true?1:2" "Right (JSExpression [JSExpressionTernary [JSLiteral \"true\"] [JSDecimal \"1\"] [JSDecimal \"2\"]])")

    , testCase "Statement4"        (testStmt "x||y"     "Right (JSExpression [JSExpressionBinary \"||\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement5"        (testStmt "x&&y"     "Right (JSExpression [JSExpressionBinary \"&&\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")      
    , testCase "Statement6"        (testStmt "x|y"     "Right (JSExpression [JSExpressionBinary \"|\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement6"        (testStmt "x^y"     "Right (JSExpression [JSExpressionBinary \"^\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement7"        (testStmt "x&y"     "Right (JSExpression [JSExpressionBinary \"&\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
      
    , testCase "Statement8"        (testStmt "x==y"     "Right (JSExpression [JSExpressionBinary \"==\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement9"        (testStmt "x!=y"     "Right (JSExpression [JSExpressionBinary \"!=\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")      
    , testCase "Statement10"       (testStmt "x===y"     "Right (JSExpression [JSExpressionBinary \"===\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement11"       (testStmt "x!==y"     "Right (JSExpression [JSExpressionBinary \"!==\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")      
      
    , testCase "Statement12"       (testStmt "x<y"     "Right (JSExpression [JSExpressionBinary \"<\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement12"       (testStmt "x>y"     "Right (JSExpression [JSExpressionBinary \">\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement12"       (testStmt "x<=y"     "Right (JSExpression [JSExpressionBinary \"<=\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement12"       (testStmt "x>=y"     "Right (JSExpression [JSExpressionBinary \">=\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    -- , testCase "Statement12"       (testStmt "x instanceof y"     "") -- TODO: restore test case
      
    , testCase "Statement13"       (testStmt "x<<y"     "Right (JSExpression [JSExpressionBinary \"<<\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])") 
    , testCase "Statement13"       (testStmt "x>>y"     "Right (JSExpression [JSExpressionBinary \">>\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])") 
    , testCase "Statement13"       (testStmt "x>>>y"     "Right (JSExpression [JSExpressionBinary \">>>\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])") 
      
    , testCase "Statement14"       (testStmt "x+y"     "Right (JSExpression [JSExpressionBinary \"+\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement14"       (testStmt "x-y"     "Right (JSExpression [JSExpressionBinary \"-\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
      
    , testCase "Statement15"       (testStmt "x*y"     "Right (JSExpression [JSExpressionBinary \"*\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement15"       (testStmt "x/y"     "Right (JSExpression [JSExpressionBinary \"/\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement15"       (testStmt "x%y"     "Right (JSExpression [JSExpressionBinary \"%\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
      
    , testCase "Statement16"       (testStmt "delete y"  "Right (JSExpression [JSUnary \"delete \",JSIdentifier \"y\"])")      
    , testCase "Statement16"       (testStmt "void y"    "Right (JSExpression [JSUnary \"void \",JSIdentifier \"y\"])")      
    , testCase "Statement16"       (testStmt "typeof y"  "Right (JSExpression [JSUnary \"typeof \",JSIdentifier \"y\"])")    
    , testCase "Statement16"       (testStmt "++y"    "Right (JSExpression [JSUnary \"++\",JSIdentifier \"y\"])")      
    , testCase "Statement16"       (testStmt "--y"    "Right (JSExpression [JSUnary \"--\",JSIdentifier \"y\"])")      
    , testCase "Statement16"       (testStmt "+y"     "Right (JSExpression [JSUnary \"+\",JSIdentifier \"y\"])")      
    , testCase "Statement16"       (testStmt "-y"     "Right (JSExpression [JSUnary \"-\",JSIdentifier \"y\"])")      
    , testCase "Statement16"       (testStmt "~y"     "Right (JSExpression [JSUnary \"~\",JSIdentifier \"y\"])")      
    , testCase "Statement16"       (testStmt "!y"     "Right (JSExpression [JSUnary \"!\",JSIdentifier \"y\"])")      
      
    , testCase "Statement17"       (testStmt "y++"     "Right (JSExpression [JSExpressionPostfix \"++\" [JSIdentifier \"y\"]])")
    , testCase "Statement17"       (testStmt "y--"     "Right (JSExpression [JSExpressionPostfix \"--\" [JSIdentifier \"y\"]])")      
      
      -- Member Expressions
    , testCase "MemberExpression1" (testStmt "function(){}"    "Right (JSExpression [JSFunctionExpression [] (JSFunctionBody [])])")
    , testCase "MemberExpression1" (testStmt "function(a){}"    "Right (JSExpression [JSFunctionExpression [JSIdentifier \"a\"] (JSFunctionBody [])])")
    , testCase "MemberExpression1" (testStmt "function(a,b){}"  "Right (JSExpression [JSFunctionExpression [JSIdentifier \"a\",JSIdentifier \"b\"] (JSFunctionBody [])])")
      
    , testCase "MemberExpression1" (testStmt "x[y]"  "Right (JSExpression [JSMemberSquare (JSExpression [JSIdentifier \"y\"]) [JSIdentifier \"x\"]])")
    , testCase "MemberExpression1" (testStmt "x[y][z]"  "Right (JSExpression [JSMemberSquare (JSExpression [JSIdentifier \"z\"]) [JSMemberSquare (JSExpression [JSIdentifier \"y\"]) [JSIdentifier \"x\"]]])")
    , testCase "MemberExpression1" (testStmt "x.y"      "Right (JSExpression [JSMemberDot [JSIdentifier \"x\",JSIdentifier \"y\"]])")
    , testCase "MemberExpression1" (testStmt "x.y.z"    "Right (JSExpression [JSMemberDot [JSMemberDot [JSIdentifier \"x\",JSIdentifier \"y\"],JSIdentifier \"z\"]])")
      
    , testCase "MemberExpression1" (testStmt "new x()"  "Right (JSExpression [JSLiteral \"new \",JSIdentifier \"x\",JSArguments []])")
      
    , testCase "NewExpression1" (testStmt "new x.y"  "Right (JSExpression [JSLiteral \"new\",JSMemberDot [JSIdentifier \"x\",JSIdentifier \"y\"]])")
      
    , testCase "CallExpression1" (testStmt "x()"     "Right (JSExpression [JSIdentifier \"x\",JSArguments []])")
    , testCase "CallExpression2" (testStmt "x()()"   "Right (JSExpression [JSIdentifier \"x\",JSArguments [],JSCallExpression \"()\" [JSArguments []]])")
    , testCase "CallExpression3" (testStmt "x()[4]"  "Right (JSExpression [JSIdentifier \"x\",JSArguments [],JSCallExpression \"[]\" [JSExpression [JSDecimal \"4\"]]])")
    , testCase "CallExpression4" (testStmt "x().x"   "Right (JSExpression [JSIdentifier \"x\",JSArguments [],JSCallExpression \".\" [JSIdentifier \"x\"]])")
    , testCase "CallExpression5" (testStmt "x(a,b=2).x"   "Right (JSExpression [JSIdentifier \"x\",JSArguments [[JSIdentifier \"a\"],[JSElement \"assignmentExpression\" [JSIdentifier \"b\",JSOperator \"=\",JSDecimal \"2\"]]],JSCallExpression \".\" [JSIdentifier \"x\"]])")
          
    , testCase "AssignExpression1" (testStmt "x=1"   "Right (JSExpression [JSElement \"assignmentExpression\" [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"]])")
    , testCase "AssignExpression1" (testStmt "x*=1"   "Right (JSExpression [JSElement \"assignmentExpression\" [JSIdentifier \"x\",JSOperator \"*=\",JSDecimal \"1\"]])")      
    , testCase "AssignExpression1" (testStmt "x/=1"   "Right (JSExpression [JSElement \"assignmentExpression\" [JSIdentifier \"x\",JSOperator \"/=\",JSDecimal \"1\"]])")      
    , testCase "AssignExpression1" (testStmt "x%=1"   "Right (JSExpression [JSElement \"assignmentExpression\" [JSIdentifier \"x\",JSOperator \"%=\",JSDecimal \"1\"]])")      
    , testCase "AssignExpression1" (testStmt "x+=1"   "Right (JSExpression [JSElement \"assignmentExpression\" [JSIdentifier \"x\",JSOperator \"+=\",JSDecimal \"1\"]])")      
    , testCase "AssignExpression1" (testStmt "x-=1"   "Right (JSExpression [JSElement \"assignmentExpression\" [JSIdentifier \"x\",JSOperator \"-=\",JSDecimal \"1\"]])")      
    , testCase "AssignExpression1" (testStmt "x<<=1"   "Right (JSExpression [JSElement \"assignmentExpression\" [JSIdentifier \"x\",JSOperator \"<<=\",JSDecimal \"1\"]])")      
    , testCase "AssignExpression1" (testStmt "x>>=1"   "Right (JSExpression [JSElement \"assignmentExpression\" [JSIdentifier \"x\",JSOperator \">>=\",JSDecimal \"1\"]])")      
    , testCase "AssignExpression1" (testStmt "x>>>=1"   "Right (JSExpression [JSElement \"assignmentExpression\" [JSIdentifier \"x\",JSOperator \">>>=\",JSDecimal \"1\"]])")      
    , testCase "AssignExpression1" (testStmt "x&=1"   "Right (JSExpression [JSElement \"assignmentExpression\" [JSIdentifier \"x\",JSOperator \"&=\",JSDecimal \"1\"]])")      
    , testCase "AssignExpression1" (testStmt "x^=1"   "Right (JSExpression [JSElement \"assignmentExpression\" [JSIdentifier \"x\",JSOperator \"^=\",JSDecimal \"1\"]])")      
    , testCase "AssignExpression1" (testStmt "x|=1"   "Right (JSExpression [JSElement \"assignmentExpression\" [JSIdentifier \"x\",JSOperator \"|=\",JSDecimal \"1\"]])")      
    
      
    , testCase "Block1" (testStmt "{}"     "Right (JSBlock (JSStatementList []))")
    , testCase "Block2" (testStmt "{x=1}"  "Right (JSBlock (JSStatementList [JSStatementList [JSExpression [JSElement \"assignmentExpression\" [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"]]]]))")
    , testCase "Block3" (testStmt "{x=1;y=2}"   "Right (JSBlock (JSStatementList [JSStatementList [JSExpression [JSElement \"assignmentExpression\" [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"]],JSLiteral \";\",JSExpression [JSElement \"assignmentExpression\" [JSIdentifier \"y\",JSOperator \"=\",JSDecimal \"2\"]]]]))")
      
    , testCase "If1" (testStmt "if (1) {}"   "Right (JSIf (JSExpression [JSDecimal \"1\"]) (JSBlock (JSStatementList [])))")      
      
    , testCase "IfElse1" (testStmt "if (1) {} else {}"   "Right (JSIfElse (JSExpression [JSDecimal \"1\"]) (JSBlock (JSStatementList [])) (JSBlock (JSStatementList [])))")
      
    , testCase "DoWhile1" (testStmt "do {x=1} while (true);"   "Right (JSDoWhile (JSBlock (JSStatementList [JSStatementList [JSExpression [JSElement \"assignmentExpression\" [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"]]]])) (JSExpression [JSLiteral \"true\"]) (JSLiteral \";\"))")
    , testCase "While1"   (testStmt "while(true);"             "Right (JSWhile (JSExpression [JSLiteral \"true\"]) (JSLiteral \";\"))")
      
    , testCase "For1"   (testStmt "for(;;);"             "Right (JSFor [] [] [] (JSLiteral \";\"))")
    , testCase "For2"   (testStmt "for(x=1;x<10;x++);"   "Right (JSFor [JSExpression [JSElement \"assignmentExpression\" [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"]]] [JSExpression [JSExpressionBinary \"<\" [JSIdentifier \"x\"] [JSDecimal \"10\"]]] [JSExpression [JSExpressionPostfix \"++\" [JSIdentifier \"x\"]]] (JSLiteral \";\"))")
      
    , testCase "ForVar1"   (testStmt "for(var x;;);"            "Right (JSForVar [JSVarDecl (JSIdentifier \"x\") []] [] [] (JSLiteral \";\"))")
    , testCase "ForVar2"   (testStmt "for(var x=1;;);"          "Right (JSForVar [JSVarDecl (JSIdentifier \"x\") [JSDecimal \"1\"]] [] [] (JSLiteral \";\"))")      
    , testCase "ForVar2"   (testStmt "for(var x;y;z){}"         "Right (JSForVar [JSVarDecl (JSIdentifier \"x\") []] [JSExpression [JSIdentifier \"y\"]] [JSExpression [JSIdentifier \"z\"]] (JSBlock (JSStatementList [])))")      
      
    , testCase "ForIn1"   (testStmt "for(x in 5){}"         "Right (JSForIn [JSIdentifier \"x\"] (JSExpression [JSDecimal \"5\"]) (JSBlock (JSStatementList [])))")
    
    , testCase "ForVarIn1" (testStmt "for(var x in 5){}"    "Right (JSForVarIn (JSVarDecl (JSIdentifier \"x\") []) (JSExpression [JSDecimal \"5\"]) (JSBlock (JSStatementList [])))")
      
    , testCase "Var1" (testStmt "var x=1;"        "Right (JSVariables \"var\" [JSVarDecl (JSIdentifier \"x\") [JSDecimal \"1\"]])")
    , testCase "Var2" (testStmt "const x=1,y=2;"  "Right (JSVariables \"const\" [JSVarDecl (JSIdentifier \"x\") [JSDecimal \"1\"],JSVarDecl (JSIdentifier \"y\") [JSDecimal \"2\"]])")
      
    , testCase "Continue1" (testStmt "continue;"       "Right (JSContinue [JSLiteral \";\"])")
    , testCase "Continue2" (testStmt "continue x;"     "Right (JSContinue [JSIdentifier \"x\",JSLiteral \";\"])")
      
    , testCase "Break1" (testStmt "break;"       "Right (JSBreak [] [JSLiteral \";\"])")
    , testCase "Break2" (testStmt "break x;"     "Right (JSBreak [JSIdentifier \"x\"] [JSLiteral \";\"])")
    
    , testCase "Return1" (testStmt "return;"       "Right (JSReturn [JSLiteral \";\"])")
    , testCase "Return2" (testStmt "return x;"     "Right (JSReturn [JSExpression [JSIdentifier \"x\"],JSLiteral \";\"])")
    
    , testCase "With1" (testStmt "with (x) {};"    "Right (JSWith (JSExpression [JSIdentifier \"x\"]) [JSBlock (JSStatementList []),JSLiteral \";\"])")
      
    , testCase "Labelled1" (testStmt "abc:x=1"    "Right (JSLabelled (JSIdentifier \"abc\") (JSExpression [JSElement \"assignmentExpression\" [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"]]))")
      
    , testCase "Switch1" (testStmt "switch (x) {}" "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) [])")
    , testCase "Switch2" (testStmt "switch (x) {case 1:break;}"          "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) [JSCase (JSExpression [JSDecimal \"1\"]) (JSStatementList [JSBreak [] [JSLiteral \";\"]])])")
    , testCase "Switch3" (testStmt "switch (x) {case 0:\ncase 1:break;}"  "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) [JSCase (JSExpression [JSDecimal \"0\"]) (JSStatementList []),JSCase (JSExpression [JSDecimal \"1\"]) (JSStatementList [JSBreak [] [JSLiteral \";\"]])])")                          
    , testCase "Switch4" (testStmt "switch (x) {default:break;}"          "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) [JSDefault (JSStatementList [JSBreak [] [JSLiteral \";\"]])])")
    , testCase "Switch5" (testStmt "switch (x) {default:\ncase 1:break;}"   "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) [JSDefault (JSStatementList []),JSCase (JSExpression [JSDecimal \"1\"]) (JSStatementList [JSBreak [] [JSLiteral \";\"]])])")
      
    , testCase "Throw1" (testStmt "throw 1"   "Right (JSThrow (JSExpression [JSDecimal \"1\"]))")
      
    , testCase "Try1" (testStmt "try{}catch(a){}"            "Right (JSTry (JSBlock (JSStatementList [])) [JSCatch (JSIdentifier \"a\") [] (JSBlock (JSStatementList []))])")
    , testCase "Try2" (testStmt "try{}finally{}"             "Right (JSTry (JSBlock (JSStatementList [])) [JSFinally (JSBlock (JSStatementList []))])")
    , testCase "Try3" (testStmt "try{}catch(a){}finally{}"   "Right (JSTry (JSBlock (JSStatementList [])) [JSCatch (JSIdentifier \"a\") [] (JSBlock (JSStatementList [])),JSFinally (JSBlock (JSStatementList []))])")
    -- TODO: add syntax extensions tests to Try  
      
    ]

srcHelloWorld = "Hello"
caseHelloWorld =  
  "Right (JSIdentifier \"Hello\")"
  @=? (show $ parseStmt srcHelloWorld "src")
  

-- ---------------------------------------------------------------------
-- Test utilities

testLiteral literal expected = expected @=? (show $ parseUsing parseLiteral literal "src")

testPE str expected = expected @=? (show $ parseUsing parsePrimaryExpression str "src")

testStmt str expected = expected @=? (show $ parseUsing parseStatement str "src")


-- EOF
