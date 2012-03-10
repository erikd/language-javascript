import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)


import Control.Monad (liftM)
import Language.JavaScript.Parser.Parser
import Language.JavaScript.Parser
--import Language.JavaScript.Parser.Grammar
import Language.JavaScript.Parser.Grammar5

main :: IO ()
main = defaultMain [testSuite,{- ++AZ++temporary++ commentSuite ,-}commentPrintSuite]

one :: IO ()
one = defaultMain [oneSuite]

oneSuite :: Test
oneSuite = testGroup "One"
 [
   testCase "ObjectLiteral7"    (testProg "x={get foo() {return 1},set foo(a) {x=a}}"  "")
 ]

testSuite :: Test
testSuite = testGroup "Parser"
    [
      testCase "helloWorld"        caseHelloWorld
    , testCase "LiteralNull"       (testLiteral "null"     "Right (JSLiteral \"null\")")
    , testCase "LiteralFalse"      (testLiteral "false"    "Right (JSLiteral \"false\")")
    , testCase "LiteralTrue"       (testLiteral "true"     "Right (JSLiteral \"true\")")
    , testCase "LiteralHexInteger1" (testLiteral "0x1234fF" "Right (JSHexInteger \"0x1234fF\")")
    , testCase "LiteralHexInteger2" (testLiteral "0X1234fF" "Right (JSHexInteger \"0X1234fF\")")
    , testCase "LiteralDecimal1"   (testLiteral "1.0e4"    "Right (JSDecimal \"1.0e4\")")
    , testCase "LiteralDecimal2"   (testLiteral "2.3E6"    "Right (JSDecimal \"2.3E6\")")
    , testCase "LiteralDecimal3"   (testLiteral "4.5"      "Right (JSDecimal \"4.5\")")
    , testCase "LiteralDecimal3"   (testLiteral "0.7e8"    "Right (JSDecimal \"0.7e8\")")
    , testCase "LiteralDecimal4"   (testLiteral "0.7E8"    "Right (JSDecimal \"0.7E8\")")
    , testCase "LiteralDecimal5"   (testLiteral "10"       "Right (JSDecimal \"10\")")
    , testCase "LiteralDecimal6"   (testLiteral "0"        "Right (JSDecimal \"0\")")
    , testCase "LiteralDecimal7"   (testLiteral "0.03"     "Right (JSDecimal \"0.03\")")
    , testCase "LiteralDecimal9"   (testLiteral "0.7e+8"   "Right (JSDecimal \"0.7e+8\")")
    , testCase "LiteralDecimal10"  (testLiteral "0.7e-18"  "Right (JSDecimal \"0.7e-18\")")
    , testCase "LiteralDecimal11"  (testLiteral "1.0e+4"   "Right (JSDecimal \"1.0e+4\")")
    , testCase "LiteralDecimal12"  (testLiteral "1.0e-4"   "Right (JSDecimal \"1.0e-4\")")
    , testCase "LiteralDecimal13"  (testLiteral "1e18"     "Right (JSDecimal \"1e18\")")
    , testCase "LiteralDecimal14"  (testLiteral "1e+18"    "Right (JSDecimal \"1e+18\")")
    , testCase "LiteralDecimal15"  (testLiteral "1e-18"    "Right (JSDecimal \"1e-18\")")

    , testCase "LiteralString1"    (testLiteral "\"hello\\nworld\"" "Right (JSStringLiteral '\"' \"hello\\\\nworld\")")
    , testCase "LiteralString2"    (testLiteral "'hello\\nworld'"  "Right (JSStringLiteral '\\'' \"hello\\\\nworld\")")

    , testCase "LiteralThis"       (testPE "this"  "Right (JSLiteral \"this\")")

    , testCase "LiteralRegex1"     (testPE "/blah/"  "Right (JSRegEx \"/blah/\")")
    , testCase "LiteralRegex2"     (testPE "/$/g"    "Right (JSRegEx \"/$/g\")")
    , testCase "LiteralRegex3"     (testPE "/\\n/g"  "Right (JSRegEx \"/\\\\n/g\")")
    , testCase "LiteralRegex4"     (testPE "/^\"(?:\\.|[^\"])*\"|^'(?:[^']|\\.)*'/" "Right (JSRegEx \"/^\\\"(?:\\\\.|[^\\\"])*\\\"|^'(?:[^']|\\\\.)*'/\")")


    , testCase "Identifier1"       (testPE "_$"      "Right (JSIdentifier \"_$\")")
    , testCase "Identifier2"       (testPE "this_"   "Right (JSIdentifier \"this_\")")

    , testCase "ArrayLiteral1"     (testPE "[]"      "Right (JSArrayLiteral [])")
    , testCase "ArrayLiteral2"     (testPE "[,]"     "Right (JSArrayLiteral [JSElision []])")
    , testCase "ArrayLiteral3"     (testPE "[,,]"    "Right (JSArrayLiteral [JSElision [],JSElision []])")
    , testCase "ArrayLiteral4"     (testPE "[,,x]"   "Right (JSArrayLiteral [JSElision [],JSElision [],JSIdentifier \"x\"])")
    , testCase "ArrayLiteral5"     (testPE "[,,x]"   "Right (JSArrayLiteral [JSElision [],JSElision [],JSIdentifier \"x\"])")
    , testCase "ArrayLiteral6"     (testPE "[,x,,x]" "Right (JSArrayLiteral [JSElision [],JSIdentifier \"x\",JSElision [],JSElision [],JSIdentifier \"x\"])")
    , testCase "ArrayLiteral7"     (testPE "[x]"     "Right (JSArrayLiteral [JSIdentifier \"x\"])")
    , testCase "ArrayLiteral8"     (testPE "[x,]"    "Right (JSArrayLiteral [JSIdentifier \"x\",JSLiteral \",\"])")

    , testCase "ObjectLiteral1"    (testPE "{}"       "Right (JSObjectLiteral [])")
    , testCase "ObjectLiteral2"    (testPE "{x:1}"    "Right (JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"x\") [JSDecimal \"1\"]])")
    , testCase "ObjectLiteral3"    (testPE "{x:1,y:2}"     "Right (JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"x\") [JSDecimal \"1\"],JSPropertyNameandValue (JSIdentifier \"y\") [JSDecimal \"2\"]])")

    , testCase "ObjectLiteral4"    (testPE "{evaluate:evaluate,load:function load(s){if(x)return s;1}}" "Right (JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"evaluate\") [JSIdentifier \"evaluate\"],JSPropertyNameandValue (JSIdentifier \"load\") [JSFunctionExpression [JSIdentifier \"load\"] [JSIdentifier \"s\"] (JSFunctionBody [JSSourceElements [JSIf (JSExpression [JSIdentifier \"x\"]) (JSReturn [JSExpression [JSIdentifier \"s\"],JSLiteral \";\"]),JSExpression [JSDecimal \"1\"]]])]])")

    , testCase "ObjectLiteral5"    (testPE "{x:1,}"    "Right (JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"x\") [JSDecimal \"1\"],JSLiteral \",\"])")

    , testCase "ObjectLiteral6"    (testProg "a={\n  values: 7,\n}\n" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"a\",JSOperator \"=\",JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"values\") [JSDecimal \"7\"],JSLiteral \",\"]]])")

    -- Edition 5 extensions
    , testCase "ObjectLiteral7"    (testProg "x={get foo() {return 1},set foo(a) {x=a}}"  "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSObjectLiteral [JSPropertyAccessor \"get\" (JSIdentifier \"foo\") [] (JSFunctionBody [JSSourceElements [JSReturn [JSExpression [JSDecimal \"1\"],JSLiteral \"\"]]]),JSPropertyAccessor \"set\" (JSIdentifier \"foo\") [JSIdentifier \"a\"] (JSFunctionBody [JSSourceElements [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSIdentifier \"a\"]]])]]])")

    , testCase "ObjectLiteral8"    (testProg "a={if:1,interface:2}" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"a\",JSOperator \"=\",JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"if\") [JSDecimal \"1\"],JSPropertyNameandValue (JSIdentifier \"interface\") [JSDecimal \"2\"]]]])")

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
    , testCase "Statement16"       (testStmt "x/y"     "Right (JSExpression [JSExpressionBinary \"/\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement17"       (testStmt "x%y"     "Right (JSExpression [JSExpressionBinary \"%\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")

    , testCase "Statement18"       (testStmt "delete y"  "Right (JSExpression [JSUnary \"delete \",JSIdentifier \"y\"])")
    , testCase "Statement19"       (testStmt "void y"    "Right (JSExpression [JSUnary \"void \",JSIdentifier \"y\"])")
    , testCase "Statement20"       (testStmt "typeof y"  "Right (JSExpression [JSUnary \"typeof \",JSIdentifier \"y\"])")
    , testCase "Statement21"       (testStmt "++y"    "Right (JSExpression [JSUnary \"++\",JSIdentifier \"y\"])")
    , testCase "Statement22"       (testStmt "--y"    "Right (JSExpression [JSUnary \"--\",JSIdentifier \"y\"])")
    , testCase "Statement23"       (testStmt "+y"     "Right (JSExpression [JSUnary \"+\",JSIdentifier \"y\"])")
    , testCase "Statement24"       (testStmt "-y"     "Right (JSExpression [JSUnary \"-\",JSIdentifier \"y\"])")
    , testCase "Statement25"       (testStmt "~y"     "Right (JSExpression [JSUnary \"~\",JSIdentifier \"y\"])")
    , testCase "Statement26"       (testStmt "!y"     "Right (JSExpression [JSUnary \"!\",JSIdentifier \"y\"])")

    , testCase "Statement27"       (testStmt "y++"     "Right (JSExpression [JSExpressionPostfix \"++\" [JSIdentifier \"y\"]])")
    , testCase "Statement28"       (testStmt "y--"     "Right (JSExpression [JSExpressionPostfix \"--\" [JSIdentifier \"y\"]])")

      -- Member Expressions
    , testCase "MemberExpression1" (testStmt "function(){}"    "Right (JSExpression [JSFunctionExpression [] [] (JSFunctionBody [])])")
    , testCase "MemberExpression1" (testStmt "function(a){}"    "Right (JSExpression [JSFunctionExpression [] [JSIdentifier \"a\"] (JSFunctionBody [])])")
    , testCase "MemberExpression1" (testStmt "function(a,b){}"  "Right (JSExpression [JSFunctionExpression [] [JSIdentifier \"a\",JSIdentifier \"b\"] (JSFunctionBody [])])")

    , testCase "MemberExpression1" (testStmt "x[y]"     "Right (JSExpression [JSMemberSquare [JSIdentifier \"x\"] (JSExpression [JSIdentifier \"y\"])])")
    , testCase "MemberExpression1" (testStmt "x[y][z]"  "Right (JSExpression [JSMemberSquare [JSMemberSquare [JSIdentifier \"x\"] (JSExpression [JSIdentifier \"y\"])] (JSExpression [JSIdentifier \"z\"])])")
    , testCase "MemberExpression1" (testStmt "x.y"      "Right (JSExpression [JSMemberDot [JSIdentifier \"x\"] (JSIdentifier \"y\")])")
    , testCase "MemberExpression1" (testStmt "x.y.z"    "Right (JSExpression [JSMemberDot [JSMemberDot [JSIdentifier \"x\"] (JSIdentifier \"y\")] (JSIdentifier \"z\")])")

    , testCase "MemberExpression1" (testStmt "new x()"  "Right (JSExpression [JSLiteral \"new \",JSIdentifier \"x\",JSArguments []])")

    , testCase "NewExpression1" (testStmt "new x.y"  "Right (JSExpression [JSLiteral \"new \",JSMemberDot [JSIdentifier \"x\"] (JSIdentifier \"y\")])")

    , testCase "CallExpression1" (testStmt "x()"     "Right (JSExpression [JSIdentifier \"x\",JSArguments []])")
    , testCase "CallExpression2" (testStmt "x()()"   "Right (JSExpression [JSIdentifier \"x\",JSArguments [],JSCallExpression \"()\" [JSArguments []]])")
    , testCase "CallExpression3" (testStmt "x()[4]"  "Right (JSExpression [JSIdentifier \"x\",JSArguments [],JSCallExpression \"[]\" [JSExpression [JSDecimal \"4\"]]])")
    , testCase "CallExpression4" (testStmt "x().x"   "Right (JSExpression [JSIdentifier \"x\",JSArguments [],JSCallExpression \".\" [JSIdentifier \"x\"]])")
    , testCase "CallExpression5" (testStmt "x(a,b=2).x"   "Right (JSExpression [JSIdentifier \"x\",JSArguments [[JSIdentifier \"a\"],[JSIdentifier \"b\",JSOperator \"=\",JSDecimal \"2\"]],JSCallExpression \".\" [JSIdentifier \"x\"]])")

    , testCase "AssignExpression1" (testStmt "x=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x*=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"*=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x/=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"/=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x%=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"%=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x+=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"+=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x-=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"-=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x<<=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"<<=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x>>=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \">>=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x>>>=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \">>>=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x&=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"&=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x^=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"^=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x|=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"|=\",JSDecimal \"1\"])")


    , testCase "Block1" (testStmt "{}"     "Right (JSStatementBlock (JSStatementList []))")
    , testCase "Block2" (testStmt "{x=1}"  "Right (JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"]]))")
    , testCase "Block3" (testStmt "{x=1;y=2}"   "Right (JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\",JSExpression [JSIdentifier \"y\",JSOperator \"=\",JSDecimal \"2\"]]))")
    , testCase "Block4" (testStmt "{{}}"     "Right (JSStatementBlock (JSStatementList [JSStatementBlock (JSStatementList [])]))")
    , testCase "Block5" (testStmt "{{{}}}"   "Right (JSStatementBlock (JSStatementList [JSStatementBlock (JSStatementList [JSStatementBlock (JSStatementList [])])]))")

    , testCase "If1" (testStmt "if (1) {}"   "Right (JSIf (JSExpression [JSDecimal \"1\"]) (JSStatementBlock (JSStatementList [])))")

    , testCase "IfElse1" (testStmt "if (1) {} else {}"   "Right (JSIfElse (JSExpression [JSDecimal \"1\"]) (JSStatementBlock (JSStatementList [])) (JSStatementBlock (JSStatementList [])))")
    , testCase "IfElse2" (testStmt "if (1) x=1; else {}" "Right (JSIfElse (JSExpression [JSDecimal \"1\"]) (JSBlock (JSStatementList [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"]])) (JSStatementBlock (JSStatementList [])))")

    , testCase "DoWhile1" (testStmt "do {x=1} while (true);"   "Right (JSDoWhile (JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"]])) (JSExpression [JSLiteral \"true\"]) (JSLiteral \";\"))")
    , testCase "While1"   (testStmt "while(true);"             "Right (JSWhile (JSExpression [JSLiteral \"true\"]) (JSLiteral \";\"))")

    , testCase "For1"   (testStmt "for(;;);"             "Right (JSFor [] [] [] (JSLiteral \";\"))")
    , testCase "For2"   (testStmt "for(x=1;x<10;x++);"   "Right (JSFor [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"]] [JSExpression [JSExpressionBinary \"<\" [JSIdentifier \"x\"] [JSDecimal \"10\"]]] [JSExpression [JSExpressionPostfix \"++\" [JSIdentifier \"x\"]]] (JSLiteral \";\"))")

    , testCase "ForVar1"   (testStmt "for(var x;;);"            "Right (JSForVar [JSVarDecl (JSIdentifier \"x\") []] [] [] (JSLiteral \";\"))")
    , testCase "ForVar2"   (testStmt "for(var x=1;;);"          "Right (JSForVar [JSVarDecl (JSIdentifier \"x\") [JSDecimal \"1\"]] [] [] (JSLiteral \";\"))")
    , testCase "ForVar2"   (testStmt "for(var x;y;z){}"         "Right (JSForVar [JSVarDecl (JSIdentifier \"x\") []] [JSExpression [JSIdentifier \"y\"]] [JSExpression [JSIdentifier \"z\"]] (JSStatementBlock (JSStatementList [])))")

    , testCase "ForIn1"   (testStmt "for(x in 5){}"         "Right (JSForIn [JSIdentifier \"x\"] (JSExpression [JSDecimal \"5\"]) (JSStatementBlock (JSStatementList [])))")

    , testCase "ForVarIn1" (testStmt "for(var x in 5){}"    "Right (JSForVarIn (JSVarDecl (JSIdentifier \"x\") []) (JSExpression [JSDecimal \"5\"]) (JSStatementBlock (JSStatementList [])))")

    , testCase "Var1" (testStmt "var x=1;"        "Right (JSVariables \"var\" [JSVarDecl (JSIdentifier \"x\") [JSDecimal \"1\"]])")
    , testCase "Var2" (testStmt "const x=1,y=2;"  "Right (JSVariables \"const\" [JSVarDecl (JSIdentifier \"x\") [JSDecimal \"1\"],JSVarDecl (JSIdentifier \"y\") [JSDecimal \"2\"]])")

    , testCase "Continue1" (testStmt "continue;"       "Right (JSContinue [JSLiteral \";\"])")
    , testCase "Continue2" (testStmt "continue x;"     "Right (JSContinue [JSIdentifier \"x\",JSLiteral \";\"])")

    , testCase "Break1" (testStmt "break;"       "Right (JSBreak [] [JSLiteral \";\"])")
    , testCase "Break2" (testStmt "break x;"     "Right (JSBreak [JSIdentifier \"x\"] [JSLiteral \";\"])")

    , testCase "Return1" (testStmt "return;"       "Right (JSReturn [JSLiteral \";\"])")
    , testCase "Return2" (testStmt "return x;"     "Right (JSReturn [JSExpression [JSIdentifier \"x\"],JSLiteral \";\"])")

    , testCase "With1" (testStmt "with (x) {};"    "Right (JSWith (JSExpression [JSIdentifier \"x\"]) [JSStatementBlock (JSStatementList []),JSLiteral \";\"])")

    , testCase "Labelled1" (testStmt "abc:x=1"    "Right (JSLabelled (JSIdentifier \"abc\") (JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"]))")

    , testCase "Switch1" (testStmt "switch (x) {}" "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) [JSLiteral \"\"])")
    , testCase "Switch2" (testStmt "switch (x) {case 1:break;}"          "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) [JSCase (JSExpression [JSDecimal \"1\"]) (JSStatementList [JSBreak [] [JSLiteral \";\"]])])")
    , testCase "Switch3" (testStmt "switch (x) {case 0:\ncase 1:break;}"  "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) [JSCase (JSExpression [JSDecimal \"0\"]) (JSStatementList []),JSCase (JSExpression [JSDecimal \"1\"]) (JSStatementList [JSBreak [] [JSLiteral \";\"]])])")
    , testCase "Switch4" (testStmt "switch (x) {default:break;}"          "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) [JSLiteral \"\",JSDefault (JSStatementList [JSBreak [] [JSLiteral \";\"]]),JSLiteral \"\"])")
    , testCase "Switch5" (testStmt "switch (x) {default:\ncase 1:break;}"   "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) [JSLiteral \"\",JSDefault (JSStatementList []),JSCase (JSExpression [JSDecimal \"1\"]) (JSStatementList [JSBreak [] [JSLiteral \";\"]])])")

    , testCase "Throw1" (testStmt "throw 1"   "Right (JSThrow (JSExpression [JSDecimal \"1\"]))")

    , testCase "Try1" (testStmt "try{}catch(a){}"            "Right (JSTry (JSBlock (JSStatementList [])) [JSCatch (JSIdentifier \"a\") [] (JSBlock (JSStatementList []))])")
    , testCase "Try2" (testStmt "try{}finally{}"             "Right (JSTry (JSBlock (JSStatementList [])) [JSFinally (JSBlock (JSStatementList []))])")
    , testCase "Try3" (testStmt "try{}catch(a){}finally{}"   "Right (JSTry (JSBlock (JSStatementList [])) [JSCatch (JSIdentifier \"a\") [] (JSBlock (JSStatementList [])),JSFinally (JSBlock (JSStatementList []))])")

    , testCase "Try4" (testStmt "try{}catch(a){}catch(b){}finally{}"   "Right (JSTry (JSBlock (JSStatementList [])) [JSCatch (JSIdentifier \"a\") [] (JSBlock (JSStatementList [])),JSCatch (JSIdentifier \"b\") [] (JSBlock (JSStatementList [])),JSFinally (JSBlock (JSStatementList []))])")
    , testCase "Try5" (testStmt "try{}catch(a){}catch(b){}"            "Right (JSTry (JSBlock (JSStatementList [])) [JSCatch (JSIdentifier \"a\") [] (JSBlock (JSStatementList [])),JSCatch (JSIdentifier \"b\") [] (JSBlock (JSStatementList []))])")
    , testCase "Try6" (testStmt "try{}catch(a if true){}catch(b){}"     "Right (JSTry (JSBlock (JSStatementList [])) [JSCatch (JSIdentifier \"a\") [JSLiteral \"true\"] (JSBlock (JSStatementList [])),JSCatch (JSIdentifier \"b\") [] (JSBlock (JSStatementList []))])")

    , testCase "Function1" (testProg "function a(){}"      "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"a\") [] (JSFunctionBody [])])")
    , testCase "Function2" (testProg "function a(b,c){}"   "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"a\") [JSIdentifier \"b\",JSIdentifier \"c\"] (JSFunctionBody [])])")

    , testCase "Comment1" (testProg "//blah\nx=1;//foo\na"   "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\",JSExpression [JSIdentifier \"a\"]])")

    , testCase "Comment2" (testProg "/*x=1\ny=2\n*/z=2;//foo\na"   "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"z\",JSOperator \"=\",JSDecimal \"2\"],JSLiteral \";\",JSExpression [JSIdentifier \"a\"]])")

    , testCase "min_100_animals1" (testProg "function Animal(name){if(!name)throw new Error('Must specify an animal name');this.name=name};Animal.prototype.toString=function(){return this.name};o=new Animal(\"bob\");o.toString()==\"bob\"" "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"Animal\") [JSIdentifier \"name\"] (JSFunctionBody [JSSourceElements [JSIf (JSExpression [JSUnary \"!\",JSIdentifier \"name\"]) (JSBlock (JSStatementList [JSThrow (JSExpression [JSLiteral \"new \",JSIdentifier \"Error\",JSArguments [[JSStringLiteral '\\'' \"Must specify an animal name\"]]])])),JSExpression [JSMemberDot [JSLiteral \"this\"] (JSIdentifier \"name\"),JSOperator \"=\",JSIdentifier \"name\"]]]),JSLiteral \";\",JSExpression [JSMemberDot [JSMemberDot [JSIdentifier \"Animal\"] (JSIdentifier \"prototype\")] (JSIdentifier \"toString\"),JSOperator \"=\",JSFunctionExpression [] [] (JSFunctionBody [JSSourceElements [JSReturn [JSExpression [JSMemberDot [JSLiteral \"this\"] (JSIdentifier \"name\")],JSLiteral \"\"]]])],JSLiteral \";\",JSExpression [JSIdentifier \"o\",JSOperator \"=\",JSLiteral \"new \",JSIdentifier \"Animal\",JSArguments [[JSStringLiteral '\"' \"bob\"]]],JSLiteral \";\",JSExpression [JSExpressionBinary \"==\" [JSMemberDot [JSIdentifier \"o\"] (JSIdentifier \"toString\"),JSArguments []] [JSStringLiteral '\"' \"bob\"]]])")

    , testCase "min_100_animals2" (testProg "Animal=function(){return this.name};" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"Animal\",JSOperator \"=\",JSFunctionExpression [] [] (JSFunctionBody [JSSourceElements [JSReturn [JSExpression [JSMemberDot [JSLiteral \"this\"] (JSIdentifier \"name\")],JSLiteral \"\"]]])],JSLiteral \";\"])")

    , testCase "min_100_animals3" (testProg "if(a)x=1;y=2" "Right (JSSourceElementsTop [JSIf (JSExpression [JSIdentifier \"a\"]) (JSBlock (JSStatementList [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"]])),JSExpression [JSIdentifier \"y\",JSOperator \"=\",JSDecimal \"2\"]])")

    , testCase "min_100_animals4" (testProg "if(a)x=a()y=2" "Right (JSSourceElementsTop [JSIf (JSExpression [JSIdentifier \"a\"]) (JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSIdentifier \"a\",JSArguments []]),JSExpression [JSIdentifier \"y\",JSOperator \"=\",JSDecimal \"2\"]])")

    , testCase "05_regex"  (testProg "newlines=spaces.match(/\\n/g)" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"newlines\",JSOperator \"=\",JSMemberDot [JSIdentifier \"spaces\"] (JSIdentifier \"match\"),JSArguments [[JSRegEx \"/\\\\n/g\"]]]])")

    , testCase "05_regex2" (testProg "x=/\\n/g" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSRegEx \"/\\\\n/g\"]])")

    , testCase "05_regex3" (testProg "x=i(/[?|^&(){}\\[\\]+\\-*\\/\\.]/g,\"\\\\$&\")" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSIdentifier \"i\",JSArguments [[JSRegEx \"/[?|^&(){}\\\\[\\\\]+\\\\-*\\\\/\\\\.]/g\"],[JSStringLiteral '\"' \"\\\\\\\\$&\"]]]])")

    , testCase "05_regex4" (testProg "x=i(/^$/g,\"\\\\$&\")" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSIdentifier \"i\",JSArguments [[JSRegEx \"/^$/g\"],[JSStringLiteral '\"' \"\\\\\\\\$&\"]]]])")

   , testCase "05_regex5" (testProg "if(/^[a-z]/.test(t)){consts+=t.toUpperCase();keywords[t]=i}else consts+=(/^\\W/.test(t)?opTypeNames[t]:t);" "Right (JSSourceElementsTop [JSIfElse (JSExpression [JSMemberDot [JSRegEx \"/^[a-z]/\"] (JSIdentifier \"test\"),JSArguments [[JSIdentifier \"t\"]]]) (JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"consts\",JSOperator \"+=\",JSMemberDot [JSIdentifier \"t\"] (JSIdentifier \"toUpperCase\"),JSArguments []],JSLiteral \";\",JSExpression [JSMemberSquare [JSIdentifier \"keywords\"] (JSExpression [JSIdentifier \"t\"]),JSOperator \"=\",JSIdentifier \"i\"]])) (JSExpression [JSIdentifier \"consts\",JSOperator \"+=\",JSExpressionParen (JSExpression [JSExpressionTernary [JSMemberDot [JSRegEx \"/^\\\\W/\"] (JSIdentifier \"test\"),JSArguments [[JSIdentifier \"t\"]]] [JSMemberSquare [JSIdentifier \"opTypeNames\"] (JSExpression [JSIdentifier \"t\"])] [JSIdentifier \"t\"]])]),JSLiteral \";\"])")

   , testCase "if_semi" (testProg "if(x);x=1" "Right (JSSourceElementsTop [JSIf (JSExpression [JSIdentifier \"x\"]) (JSLiteral \";\"),JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"]])")

   , testCase "67_bob" (testProg "(match = /^\"(?:\\\\.|[^\"])*\"|^'(?:[^']|\\\\.)*'/(input))" "Right (JSSourceElementsTop [JSExpression [JSExpressionParen (JSExpression [JSIdentifier \"match\",JSOperator \"=\",JSRegEx \"/^\\\"(?:\\\\\\\\.|[^\\\"])*\\\"|^'(?:[^']|\\\\\\\\.)*'/\",JSArguments [[JSIdentifier \"input\"]]])]])")

   , testCase "122_jsexec" (testProg "v = getValue(execute(n[0], x)) in getValue(execute(n[1], x));" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"v\",JSOperator \"=\",JSExpressionBinary \" in \" [JSIdentifier \"getValue\",JSArguments [[JSIdentifier \"execute\",JSArguments [[JSMemberSquare [JSIdentifier \"n\"] (JSExpression [JSDecimal \"0\"])],[JSIdentifier \"x\"]]]]] [JSIdentifier \"getValue\",JSArguments [[JSIdentifier \"execute\",JSArguments [[JSMemberSquare [JSIdentifier \"n\"] (JSExpression [JSDecimal \"1\"])],[JSIdentifier \"x\"]]]]]],JSLiteral \";\"])")

   , testCase "bug1" (testProg "/* */\nfunction f() {\n/*  */\n}\n" "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"f\") [] (JSFunctionBody [])])")
   , testCase "bug1" (testProg "/* **/\nfunction f() {\n/*  */\n}\n" "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"f\") [] (JSFunctionBody [])])")

   , testCase "unicode1-ws" (testProg "a \f\v\t\r\n=\x00a0\x1680\x180e\x2000\x2001\x2002\x2003\x2004\x2005\x2006\x2007\x2008\x2009\x200a\x2028\x2029\x202f\x205f\x3000\&1;" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"a\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\"])")

   , testCase "unicode2-lt" (testProg "//comment\x000Ax=1;" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\"])")
   , testCase "unicode3-lt" (testProg "//comment\x000Dx=1;" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\"])")
   , testCase "unicode4-lt" (testProg "//comment\x2028x=1;" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\"])")
   , testCase "unicode5-lt" (testProg "//comment\x2029x=1;" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\"])")

   , testCase "unicode2" (testProg "àáâãäå = 1;" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"\\224\\225\\226\\227\\228\\229\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\"])")

   , testCase "unicode3" (testProg "$aà = 1;_b=2;\0065a=2"  "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"$a\\224\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\",JSExpression [JSIdentifier \"_b\",JSOperator \"=\",JSDecimal \"2\"],JSLiteral \";\",JSExpression [JSIdentifier \"Aa\",JSOperator \"=\",JSDecimal \"2\"]])")

   , testCase "unicode4" (testProg "x=\"àáâãäå\";y='\3012a\0068'" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSStringLiteral '\"' \"\\224\\225\\226\\227\\228\\229\"],JSLiteral \";\",JSExpression [JSIdentifier \"y\",JSOperator \"=\",JSStringLiteral '\\'' \"\\3012aD\"]])")

   , testCase "unicode5" (testFile "./test/Unicode.js" "JSSourceElementsTop [JSExpression [JSIdentifier \"\\224\\225\\226\\227\\228\\229\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\"]")

   , testCase "bug2.a" (testProg "function() {\nz = function /*z*/(o) {\nreturn r;\n};}" "Right (JSSourceElementsTop [JSExpression [JSFunctionExpression [] [] (JSFunctionBody [JSSourceElements [JSExpression [JSIdentifier \"z\",JSOperator \"=\",JSFunctionExpression [] [JSIdentifier \"o\"] (JSFunctionBody [JSSourceElements [JSReturn [JSExpression [JSIdentifier \"r\"],JSLiteral \";\"]]])],JSLiteral \";\"]])]])")

   , testCase "bug2.b" (testProg "function() {\nz = function z(o) {\nreturn r;\n};}" "Right (JSSourceElementsTop [JSExpression [JSFunctionExpression [] [] (JSFunctionBody [JSSourceElements [JSExpression [JSIdentifier \"z\",JSOperator \"=\",JSFunctionExpression [JSIdentifier \"z\"] [JSIdentifier \"o\"] (JSFunctionBody [JSSourceElements [JSReturn [JSExpression [JSIdentifier \"r\"],JSLiteral \";\"]]])],JSLiteral \";\"]])]])")

   -- https://github.com/alanz/hjsmin/issues/#issue/3
   , testCase "bug3" (testProg "var myLatlng = new google.maps.LatLng(56.8379100, 60.5806664);" "Right (JSSourceElementsTop [JSVariables \"var\" [JSVarDecl (JSIdentifier \"myLatlng\") [JSLiteral \"new \",JSMemberDot [JSMemberDot [JSIdentifier \"google\"] (JSIdentifier \"maps\")] (JSIdentifier \"LatLng\"),JSArguments [[JSDecimal \"56.8379100\"],[JSDecimal \"60.5806664\"]]]]])")

   -- https://github.com/alanz/hjsmin/issues/#issue/4
   , testCase "bug4" (testProg "/* * geolocation. пытаемся определить свое местоположение * если не получается то используем defaultLocation * @Param {object} map экземпляр карты * @Param {object LatLng} defaultLocation Координаты центра по умолчанию * @Param {function} callbackAfterLocation Фу-ия которая вызывается после * геолокации. Т.к запрос геолокации асинхронен */x" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\"]])")



   , testCase "02_sm.js"   (testProg "{zero}\none1;two\n{three\nfour;five;\n{\nsix;{seven;}\n}\n}" "Right (JSSourceElementsTop [JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"zero\"]]),JSExpression [JSIdentifier \"one1\"],JSLiteral \";\",JSExpression [JSIdentifier \"two\"],JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"three\"],JSExpression [JSIdentifier \"four\"],JSLiteral \";\",JSExpression [JSIdentifier \"five\"],JSLiteral \";\",JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"six\"],JSLiteral \";\",JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"seven\"],JSLiteral \";\"])])])])")

   , testCase "02_sm.js.2" (testProg "{zero}\nget;two\n{three\nfour;set;\n{\nsix;{seven;}\n}\n}" "Right (JSSourceElementsTop [JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"zero\"]]),JSExpression [JSIdentifier \"get\"],JSLiteral \";\",JSExpression [JSIdentifier \"two\"],JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"three\"],JSExpression [JSIdentifier \"four\"],JSLiteral \";\",JSExpression [JSIdentifier \"set\"],JSLiteral \";\",JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"six\"],JSLiteral \";\",JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"seven\"],JSLiteral \";\"])])])])")

   , testCase "loc1" (testProgUn "x = 1\n  y=2;" "Right (NS (JSSourceElementsTop [NS (JSExpression [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [NoComment],NS (JSOperator \"=\") (TokenPn 2 1 3) [NoComment],NS (JSDecimal \"1\") (TokenPn 4 1 5) [NoComment]]) (TokenPn 0 1 1) [],NS (JSExpression [NS (JSIdentifier \"y\") (TokenPn 8 2 3) [NoComment],NS (JSOperator \"=\") (TokenPn 9 2 4) [NoComment],NS (JSDecimal \"2\") (TokenPn 10 2 5) [NoComment]]) (TokenPn 8 2 3) [],NS (JSLiteral \";\") (TokenPn 11 2 6) [NoComment]]) (TokenPn 0 1 1) [])")

   -- https://github.com/alanz/language-javascript/issues/2
   , testCase "issue2" (testProg "var img = document.createElement('img');\nimg.src = \"mylogo.jpg\";\n$(img).click(function() {\n   alert('clicked!');\n});" "Right (JSSourceElementsTop [JSVariables \"var\" [JSVarDecl (JSIdentifier \"img\") [JSMemberDot [JSIdentifier \"document\"] (JSIdentifier \"createElement\"),JSArguments [[JSStringLiteral '\\'' \"img\"]]]],JSExpression [JSMemberDot [JSIdentifier \"img\"] (JSIdentifier \"src\"),JSOperator \"=\",JSStringLiteral '\"' \"mylogo.jpg\"],JSLiteral \";\",JSExpression [JSIdentifier \"$\",JSArguments [[JSIdentifier \"img\"]],JSCallExpression \".\" [JSIdentifier \"click\"],JSCallExpression \"()\" [JSArguments [[JSFunctionExpression [] [] (JSFunctionBody [JSSourceElements [JSExpression [JSIdentifier \"alert\",JSArguments [[JSStringLiteral '\\'' \"clicked!\"]]],JSLiteral \";\"]])]]]],JSLiteral \";\"])")


   -- Working in ECMASCRIPT 5.1 changes
   , testCase "lineTerminatorInString1" (testProg "x='abc\\\ndef';"    "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSStringLiteral '\\'' \"abcdef\"],JSLiteral \";\"])")
   , testCase "lineTerminatorInString2" (testProg "x=\"abc\\\ndef\";" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSStringLiteral '\"' \"abcdef\"],JSLiteral \";\"])")
   , testCase "lineTerminatorInString3" (testProg "x=\"abc\\\rdef\";" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSStringLiteral '\"' \"abcdef\"],JSLiteral \";\"])")
   , testCase "lineTerminatorInString4" (testProg "x=\"abc\\\x2028 def\";" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSStringLiteral '\"' \"abc def\"],JSLiteral \";\"])")
   , testCase "lineTerminatorInString5" (testProg "x=\"abc\\\x2029 def\";" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSStringLiteral '\"' \"abc def\"],JSLiteral \";\"])")
   , testCase "lineTerminatorInString6" (testProg "x=\"abc\\\r\ndef\";" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSStringLiteral '\"' \"abcdef\"],JSLiteral \";\"])")


     -- https://github.com/alanz/language-javascript/issues/4
   , testCase "issue4ok"   (testProg "var k = {\ny: somename\n}" "Right (JSSourceElementsTop [JSVariables \"var\" [JSVarDecl (JSIdentifier \"k\") [JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"y\") [JSIdentifier \"somename\"]]]]])")
   , testCase "issue4bug1" (testProg "var k = {\ny: code\n}" "Right (JSSourceElementsTop [JSVariables \"var\" [JSVarDecl (JSIdentifier \"k\") [JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"y\") [JSIdentifier \"code\"]]]]])")
   , testCase "issue4bug2" (testProg "var k = {\ny: mode\n}" "Right (JSSourceElementsTop [JSVariables \"var\" [JSVarDecl (JSIdentifier \"k\") [JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"y\") [JSIdentifier \"mode\"]]]]])")

     -- https://github.com/alanz/language-javascript/issues/5
   , testCase "issue5bug1" (testProg "x = { y: 1e8 }" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"y\") [JSDecimal \"1e8\"]]]])")
   , testCase "issue5ok2" (testProg "{ y: 1e8 }" "Right (JSSourceElementsTop [JSStatementBlock (JSStatementList [JSLabelled (JSIdentifier \"y\") (JSExpression [JSDecimal \"1e8\"])])])")
   , testCase "issue5ok3" (testProg "{ y: 18 }" "Right (JSSourceElementsTop [JSStatementBlock (JSStatementList [JSLabelled (JSIdentifier \"y\") (JSExpression [JSDecimal \"18\"])])])")
   , testCase "issue5ok4" (testProg "x = { y: 18 }" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"y\") [JSDecimal \"18\"]]]])")


    ]

srcHelloWorld = "Hello"
caseHelloWorld =
  "JSSourceElementsTop [JSExpression [JSIdentifier \"Hello\"]]"
  -- @=? (show $ parse srcHelloWorld "src")
  @=? (showStripped $ readJs srcHelloWorld)


-- ---------------------------------------------------------------------

commentSuite :: Test
commentSuite = testGroup "Comments"
    [
      testCase "helloWorld"        caseHelloWorld
    , testCase "LiteralNull"       (testLiteralC "/*a*/null"     "Right (NS (JSLiteral \"null\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"])")
    , testCase "LiteralFalse"      (testLiteralC "/*b*/false"    "Right (NS (JSLiteral \"false\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*b*/\"])")
    , testCase "LiteralTrue"       (testLiteralC "true"          "Right (NS (JSLiteral \"true\") (TokenPn 0 1 1) [NoComment])")
    , testCase "LiteralTrue"       (testLiteralC "/*c*/true"     "Right (NS (JSLiteral \"true\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*c*/\"])")

    , testCase "LiteralHexInteger" (testLiteralC "/*d*/0x1234fF" "Right (NS (JSHexInteger \"0x1234fF\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*d*/\"])")
    , testCase "LiteralDecimal"    (testLiteralC "/*e*/1.0e4"    "Right (NS (JSDecimal \"1.0e4\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*e*/\"])")
    , testCase "LiteralString1"    (testLiteralC "/*f*/\"hello\\nworld\"" "Right (NS (JSStringLiteral '\"' \"hello\\\\nworld\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*f*/\"])")
    , testCase "LiteralString2"    (testLiteralC "/*g*/'hello\\nworld'"   "Right (NS (JSStringLiteral '\\'' \"hello\\\\nworld\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*g*/\"])")


    , testCase "LiteralThis"       (testPEC "/*h*/this"  "Right (NS (JSLiteral \"this\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*h*/\"])")

    , testCase "LiteralRegex1"     (testPEC "/*i*//blah/"  "Right (NS (JSRegEx \"/blah/\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*i*/\"])")

    , testCase "Identifier2"       (testPEC "//j\nthis_"   "Right (NS (JSIdentifier \"this_\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"//j\"])")

    , testCase "ArrayLiteral1"     (testPEC "/*a*/[/*b*/]"      "Right (NS (JSArrayLiteral []) (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\",CommentA (TokenPn 6 1 7) \"/*b*/\"])")
    , testCase "ArrayLiteral2"     (testPEC "/*a*/[/*b*/,/*c*/]"     "Right (NS (JSArrayLiteral [NS (JSElision []) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\",CommentA (TokenPn 12 1 13) \"/*c*/\"])")
    , testCase "ArrayLiteral3"     (testPEC "/*a*/[/*b*/,/*c*/,/*d*/]"    "Right (NS (JSArrayLiteral [NS (JSElision []) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"],NS (JSElision []) (TokenPn 12 1 13) [CommentA (TokenPn 12 1 13) \"/*c*/\"]]) (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\",CommentA (TokenPn 18 1 19) \"/*d*/\"])")
    , testCase "ArrayLiteral4"     (testPEC "/*a*/[/*b/*,/*c*/,/*d*/x/*e*/]"   "Right (NS (JSArrayLiteral [NS (JSElision []) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b/*,/*c*/\"],NS (JSIdentifier \"x\") (TokenPn 18 1 19) [CommentA (TokenPn 18 1 19) \"/*d*/\"]]) (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\",CommentA (TokenPn 24 1 25) \"/*e*/\"])")
    , testCase "ArrayLiteral5"     (testPEC "/*a*/[/*b*/,/*c*/,/*d*/x/*e*/]"   "Right (NS (JSArrayLiteral [NS (JSElision []) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"],NS (JSElision []) (TokenPn 12 1 13) [CommentA (TokenPn 12 1 13) \"/*c*/\"],NS (JSIdentifier \"x\") (TokenPn 18 1 19) [CommentA (TokenPn 18 1 19) \"/*d*/\"]]) (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\",CommentA (TokenPn 24 1 25) \"/*e*/\"])")
    , testCase "ArrayLiteral6"     (testPEC "/*a*/[/*b*/,/*c*/x/*d*/,/*e*/,/*f*/x/*g*/]" "Right (NS (JSArrayLiteral [NS (JSElision []) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"],NS (JSIdentifier \"x\") (TokenPn 12 1 13) [CommentA (TokenPn 12 1 13) \"/*c*/\"],NS (JSElision []) (TokenPn 18 1 19) [CommentA (TokenPn 18 1 19) \"/*d*/\"],NS (JSElision []) (TokenPn 24 1 25) [CommentA (TokenPn 24 1 25) \"/*e*/\"],NS (JSIdentifier \"x\") (TokenPn 30 1 31) [CommentA (TokenPn 30 1 31) \"/*f*/\"]]) (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\",CommentA (TokenPn 36 1 37) \"/*g*/\"])")
    , testCase "ArrayLiteral7"     (testPEC "/*a*/[/*b*/x/*c*/]"     "Right (NS (JSArrayLiteral [NS (JSIdentifier \"x\") (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\",CommentA (TokenPn 12 1 13) \"/*c*/\"])")
    , testCase "ArrayLiteral8"     (testPEC "/*a*/[/*b*/x/*c*/,/*d*/]"    "Right (NS (JSArrayLiteral [NS (JSIdentifier \"x\") (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"],NS (JSLiteral \",\") (TokenPn 17 1 18) [CommentA (TokenPn 12 1 13) \"/*c*/\"]]) (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\",CommentA (TokenPn 18 1 19) \"/*d*/\"])")

    , testCase "ObjectLiteral1"    (testPEC "/*a*/{/*b*/}"       "Right (NS (JSObjectLiteral []) (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\",CommentA (TokenPn 6 1 7) \"/*b*/\"])")
    , testCase "ObjectLiteral2"    (testPEC "/*a*/{/*b*/x/*c*/:/*d*/1/*e*/}"    "Right (NS (JSObjectLiteral [NS (JSPropertyNameandValue (NS (JSIdentifier \"x\") (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]) [NS (JSDecimal \"1\") (TokenPn 18 1 19) [CommentA (TokenPn 18 1 19) \"/*d*/\"]]) (TokenPn 12 1 13) [CommentA (TokenPn 12 1 13) \"/*c*/\"]]) (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\",CommentA (TokenPn 24 1 25) \"/*e*/\"])")
    , testCase "ObjectLiteral3"    (testPEC "/*a*/{/*b*/x/*c*/:/*d*/1/*e*/,/*f*/y/*g*/:/*h*/2/*i*/}"     "Right (NS (JSObjectLiteral [NS (JSPropertyNameandValue (NS (JSIdentifier \"x\") (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]) [NS (JSDecimal \"1\") (TokenPn 18 1 19) [CommentA (TokenPn 18 1 19) \"/*d*/\"]]) (TokenPn 12 1 13) [CommentA (TokenPn 12 1 13) \"/*c*/\"],NS (JSPropertyNameandValue (NS (JSIdentifier \"y\") (TokenPn 30 1 31) [CommentA (TokenPn 30 1 31) \"/*f*/\"]) [NS (JSDecimal \"2\") (TokenPn 42 1 43) [CommentA (TokenPn 42 1 43) \"/*h*/\"]]) (TokenPn 36 1 37) [CommentA (TokenPn 24 1 25) \"/*e*/\",CommentA (TokenPn 36 1 37) \"/*g*/\"]]) (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\",CommentA (TokenPn 48 1 49) \"/*i*/\"])")

    , testCase "ObjectLiteral5"    (testPEC "/*a*/{/*b*/x/*c*/:/*d*/1/*e*/,/*f*/}"    "Right (NS (JSObjectLiteral [NS (JSPropertyNameandValue (NS (JSIdentifier \"x\") (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]) [NS (JSDecimal \"1\") (TokenPn 18 1 19) [CommentA (TokenPn 18 1 19) \"/*d*/\"]]) (TokenPn 12 1 13) [CommentA (TokenPn 12 1 13) \"/*c*/\"],NS (JSLiteral \",\") (TokenPn 29 1 30) [CommentA (TokenPn 24 1 25) \"/*e*/\"]]) (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\",CommentA (TokenPn 30 1 31) \"/*f*/\"])")

    -- Edition 5 extensions
    , testCase "ObjectLiteral7"    (testProgC "/*a*/x/*b*/=/*c*/{/*d*/get/*e*/ foo/*f*/(/*g*/)/*h*/ {/*i*/return/*j*/ 1/*k*/}/*l*/,/*m*/set/*n*/ foo/*o*/(/*p*/a/*q*/) /*r*/{/*s*/x/*t*/=/*u*/a/*v*/}/*w*/}"  "Right (NS (JSSourceElementsTop [NS (JSExpression [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"],NS (JSOperator \"=\") (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"],NS (JSObjectLiteral [NS (JSPropertyAccessor \"get\" (NS (JSIdentifier \"foo\") (TokenPn 26 1 27) [CommentA (TokenPn 26 1 27) \"/*e*/\"]) [] (NS (JSFunctionBody [NS (JSSourceElements [NS (JSReturn [NS (JSExpression [NS (JSDecimal \"1\") (TokenPn 65 1 66) [CommentA (TokenPn 65 1 66) \"/*j*/\"]]) (TokenPn 65 1 66) [],NS (JSLiteral \"\") (TokenPn 0 0 0) []]) (TokenPn 54 1 55) [CommentA (TokenPn 54 1 55) \"/*i*/\"]]) (TokenPn 54 1 55) []]) (TokenPn 54 1 55) [])) (TokenPn 18 1 19) [CommentA (TokenPn 18 1 19) \"/*d*/\",CommentA (TokenPn 35 1 36) \"/*f*/\",CommentA (TokenPn 41 1 42) \"/*g*/\",CommentA (TokenPn 47 1 48) \"/*h*/\",CommentA (TokenPn 72 1 73) \"/*k*/\"],NS (JSPropertyAccessor \"set\" (NS (JSIdentifier \"foo\") (TokenPn 92 1 93) [CommentA (TokenPn 92 1 93) \"/*n*/\"]) [NS (JSIdentifier \"a\") (TokenPn 107 1 108) [CommentA (TokenPn 107 1 108) \"/*p*/\"]] (NS (JSFunctionBody [NS (JSSourceElements [NS (JSExpression [NS (JSIdentifier \"x\") (TokenPn 126 1 127) [CommentA (TokenPn 126 1 127) \"/*s*/\"],NS (JSOperator \"=\") (TokenPn 132 1 133) [CommentA (TokenPn 132 1 133) \"/*t*/\"],NS (JSIdentifier \"a\") (TokenPn 138 1 139) [CommentA (TokenPn 138 1 139) \"/*u*/\"]]) (TokenPn 126 1 127) []]) (TokenPn 126 1 127) []]) (TokenPn 126 1 127) [])) (TokenPn 84 1 85) [CommentA (TokenPn 78 1 79) \"/*l*/\",CommentA (TokenPn 84 1 85) \"/*m*/\",CommentA (TokenPn 101 1 102) \"/*o*/\",CommentA (TokenPn 113 1 114) \"/*q*/\",CommentA (TokenPn 120 1 121) \"/*r*/\",CommentA (TokenPn 144 1 145) \"/*v*/\"]]) (TokenPn 12 1 13) [CommentA (TokenPn 12 1 13) \"/*c*/\",CommentA (TokenPn 150 1 151) \"/*w*/\"]]) (TokenPn 0 1 1) []]) (TokenPn 0 1 1) [])")

    , testCase "ExpressionParen"   (testPEC "/*a*/(/*b*/56/*c*/)"     "Right (NS (JSExpressionParen (NS (JSExpression [NS (JSDecimal \"56\") (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])) (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\",CommentA (TokenPn 13 1 14) \"/*c*/\"])")

    , testCase "Statement1"        (testStmtC "/*a*/x"        "Right (NS (JSExpression [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]]) (TokenPn 0 1 1) [])")

    , testCase "Statement2"        (testStmtC "/*a*/null"     "Right (NS (JSExpression [NS (JSLiteral \"null\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]]) (TokenPn 0 1 1) [])")

    , testCase "Statement3"        (testStmtC "/*a*/true/*b*/?/*c*/1/*d*/:/*e*/2" "Right (NS (JSExpression [NS (JSExpressionTernary [NS (JSLiteral \"true\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSDecimal \"1\") (TokenPn 15 1 16) [CommentA (TokenPn 15 1 16) \"/*c*/\"]] [NS (JSDecimal \"2\") (TokenPn 27 1 28) [CommentA (TokenPn 27 1 28) \"/*e*/\"]]) (TokenPn 9 1 10) [CommentA (TokenPn 9 1 10) \"/*b*/\",CommentA (TokenPn 21 1 22) \"/*d*/\"]]) (TokenPn 9 1 10) [])")

    , testCase "Statement4"        (testStmtC "/*a*/x/*b*/||/*c*/y"    "Right (NS (JSExpression [NS (JSExpressionBinary \"||\" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 13 1 14) [CommentA (TokenPn 13 1 14) \"/*c*/\"]]) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])")

    , testCase "Statement5"        (testStmtC "/*a*/x/*b*/&&/*c*/y"    "Right (NS (JSExpression [NS (JSExpressionBinary \"&&\" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 13 1 14) [CommentA (TokenPn 13 1 14) \"/*c*/\"]]) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])")

    , testCase "Statement6a"       (testStmtC "/*a*/x/*b*/|/*c*/y"     "Right (NS (JSExpression [NS (JSExpressionBinary \"|\" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 12 1 13) [CommentA (TokenPn 12 1 13) \"/*c*/\"]]) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])")

    , testCase "Statement6b"       (testStmtC "/*a*/x/*b*/^/*c*/y"     "Right (NS (JSExpression [NS (JSExpressionBinary \"^\" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 12 1 13) [CommentA (TokenPn 12 1 13) \"/*c*/\"]]) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])")

    , testCase "Statement7"        (testStmtC "/*a*/x/*b*/&/*c*/y"     "Right (NS (JSExpression [NS (JSExpressionBinary \"&\" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 12 1 13) [CommentA (TokenPn 12 1 13) \"/*c*/\"]]) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])")

    , testCase "Statement8"        (testStmtC "/*a*/x/*b*/==/*c*/y"     "Right (NS (JSExpression [NS (JSExpressionBinary \"==\" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 13 1 14) [CommentA (TokenPn 13 1 14) \"/*c*/\"]]) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])")

    , testCase "Statement9"        (testStmtC "/*a*/x/*b*/!=/*c*/y"     "Right (NS (JSExpression [NS (JSExpressionBinary \"!=\" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 13 1 14) [CommentA (TokenPn 13 1 14) \"/*c*/\"]]) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])")

    , testCase "Statement10"       (testStmtC "/*a*/x/*b*/===/*c*/y"     "Right (NS (JSExpression [NS (JSExpressionBinary \"===\" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 14 1 15) [CommentA (TokenPn 14 1 15) \"/*c*/\"]]) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])")

    , testCase "Statement11"       (testStmtC "/*a*/x/*b*/!==/*c*/y"     "Right (NS (JSExpression [NS (JSExpressionBinary \"!==\" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 14 1 15) [CommentA (TokenPn 14 1 15) \"/*c*/\"]]) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])")

    , testCase "Statement12a"       (testStmtC "/*a*/x/*b*/</*c*/y"     "Right (NS (JSExpression [NS (JSExpressionBinary \"<\" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 12 1 13) [CommentA (TokenPn 12 1 13) \"/*c*/\"]]) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])")

    , testCase "Statement12b"       (testStmtC "/*a*/x/*b*/>/*c*/y"     "Right (NS (JSExpression [NS (JSExpressionBinary \">\" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 12 1 13) [CommentA (TokenPn 12 1 13) \"/*c*/\"]]) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])")

    , testCase "Statement12c"       (testStmtC "/*a*/x/*b*/<=/*c*/y"     "Right (NS (JSExpression [NS (JSExpressionBinary \"<=\" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 13 1 14) [CommentA (TokenPn 13 1 14) \"/*c*/\"]]) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])")

    , testCase "Statement12d"       (testStmtC "/*a*/x/*b*/>=/*c*/y"     "Right (NS (JSExpression [NS (JSExpressionBinary \">=\" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 13 1 14) [CommentA (TokenPn 13 1 14) \"/*c*/\"]]) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])")

    , testCase "Statement12e"       (testStmtC "/*a*/x /*b*/instanceof /*c*/y"     "Right (NS (JSExpression [NS (JSExpressionBinary \" instanceof \" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 23 1 24) [CommentA (TokenPn 23 1 24) \"/*c*/\"]]) (TokenPn 7 1 8) [CommentA (TokenPn 7 1 8) \"/*b*/\"]]) (TokenPn 7 1 8) [])")

      {-


    , testCase "Statement13"       (testStmt "x<<y"     "Right (JSExpression [JSExpressionBinary \"<<\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement13"       (testStmt "x>>y"     "Right (JSExpression [JSExpressionBinary \">>\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement13"       (testStmt "x>>>y"     "Right (JSExpression [JSExpressionBinary \">>>\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")

    , testCase "Statement14"       (testStmt "x+y"     "Right (JSExpression [JSExpressionBinary \"+\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement14"       (testStmt "x-y"     "Right (JSExpression [JSExpressionBinary \"-\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")

    , testCase "Statement15"       (testStmt "x*y"     "Right (JSExpression [JSExpressionBinary \"*\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement16"       (testStmt "x/y"     "Right (JSExpression [JSExpressionBinary \"/\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement17"       (testStmt "x%y"     "Right (JSExpression [JSExpressionBinary \"%\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")

    , testCase "Statement18"       (testStmt "delete y"  "Right (JSExpression [JSUnary \"delete \",JSIdentifier \"y\"])")
    , testCase "Statement19"       (testStmt "void y"    "Right (JSExpression [JSUnary \"void \",JSIdentifier \"y\"])")
    , testCase "Statement20"       (testStmt "typeof y"  "Right (JSExpression [JSUnary \"typeof \",JSIdentifier \"y\"])")
    , testCase "Statement21"       (testStmt "++y"    "Right (JSExpression [JSUnary \"++\",JSIdentifier \"y\"])")
    , testCase "Statement22"       (testStmt "--y"    "Right (JSExpression [JSUnary \"--\",JSIdentifier \"y\"])")
    , testCase "Statement23"       (testStmt "+y"     "Right (JSExpression [JSUnary \"+\",JSIdentifier \"y\"])")
    , testCase "Statement24"       (testStmt "-y"     "Right (JSExpression [JSUnary \"-\",JSIdentifier \"y\"])")
    , testCase "Statement25"       (testStmt "~y"     "Right (JSExpression [JSUnary \"~\",JSIdentifier \"y\"])")
    , testCase "Statement26"       (testStmt "!y"     "Right (JSExpression [JSUnary \"!\",JSIdentifier \"y\"])")

    , testCase "Statement27"       (testStmt "y++"     "Right (JSExpression [JSExpressionPostfix \"++\" [JSIdentifier \"y\"]])")
    , testCase "Statement28"       (testStmt "y--"     "Right (JSExpression [JSExpressionPostfix \"--\" [JSIdentifier \"y\"]])")

      -- Member Expressions
    , testCase "MemberExpression1" (testStmt "function(){}"    "Right (JSExpression [JSFunctionExpression [] [] (JSFunctionBody [])])")
    , testCase "MemberExpression1" (testStmt "function(a){}"    "Right (JSExpression [JSFunctionExpression [] [JSIdentifier \"a\"] (JSFunctionBody [])])")
    , testCase "MemberExpression1" (testStmt "function(a,b){}"  "Right (JSExpression [JSFunctionExpression [] [JSIdentifier \"a\",JSIdentifier \"b\"] (JSFunctionBody [])])")

    , testCase "MemberExpression1" (testStmt "x[y]"     "Right (JSExpression [JSMemberSquare [JSIdentifier \"x\"] (JSExpression [JSIdentifier \"y\"])])")
    , testCase "MemberExpression1" (testStmt "x[y][z]"  "Right (JSExpression [JSMemberSquare [JSMemberSquare [JSIdentifier \"x\"] (JSExpression [JSIdentifier \"y\"])] (JSExpression [JSIdentifier \"z\"])])")
    , testCase "MemberExpression1" (testStmt "x.y"      "Right (JSExpression [JSMemberDot [JSIdentifier \"x\"] (JSIdentifier \"y\")])")
    , testCase "MemberExpression1" (testStmt "x.y.z"    "Right (JSExpression [JSMemberDot [JSMemberDot [JSIdentifier \"x\"] (JSIdentifier \"y\")] (JSIdentifier \"z\")])")

    , testCase "MemberExpression1" (testStmt "new x()"  "Right (JSExpression [JSLiteral \"new \",JSIdentifier \"x\",JSArguments []])")

    , testCase "NewExpression1" (testStmt "new x.y"  "Right (JSExpression [JSLiteral \"new \",JSMemberDot [JSIdentifier \"x\"] (JSIdentifier \"y\")])")

    , testCase "CallExpression1" (testStmt "x()"     "Right (JSExpression [JSIdentifier \"x\",JSArguments []])")
    , testCase "CallExpression2" (testStmt "x()()"   "Right (JSExpression [JSIdentifier \"x\",JSArguments [],JSCallExpression \"()\" [JSArguments []]])")
    , testCase "CallExpression3" (testStmt "x()[4]"  "Right (JSExpression [JSIdentifier \"x\",JSArguments [],JSCallExpression \"[]\" [JSExpression [JSDecimal \"4\"]]])")
    , testCase "CallExpression4" (testStmt "x().x"   "Right (JSExpression [JSIdentifier \"x\",JSArguments [],JSCallExpression \".\" [JSIdentifier \"x\"]])")
    , testCase "CallExpression5" (testStmt "x(a,b=2).x"   "Right (JSExpression [JSIdentifier \"x\",JSArguments [[JSIdentifier \"a\"],[JSIdentifier \"b\",JSOperator \"=\",JSDecimal \"2\"]],JSCallExpression \".\" [JSIdentifier \"x\"]])")

    , testCase "AssignExpression1" (testStmt "x=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x*=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"*=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x/=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"/=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x%=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"%=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x+=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"+=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x-=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"-=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x<<=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"<<=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x>>=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \">>=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x>>>=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \">>>=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x&=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"&=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x^=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"^=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x|=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"|=\",JSDecimal \"1\"])")


    , testCase "Block1" (testStmt "{}"     "Right (JSStatementBlock (JSStatementList []))")
    , testCase "Block2" (testStmt "{x=1}"  "Right (JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"]]))")
    , testCase "Block3" (testStmt "{x=1;y=2}"   "Right (JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\",JSExpression [JSIdentifier \"y\",JSOperator \"=\",JSDecimal \"2\"]]))")
    , testCase "Block4" (testStmt "{{}}"     "Right (JSStatementBlock (JSStatementList [JSStatementBlock (JSStatementList [])]))")
    , testCase "Block5" (testStmt "{{{}}}"   "Right (JSStatementBlock (JSStatementList [JSStatementBlock (JSStatementList [JSStatementBlock (JSStatementList [])])]))")

    , testCase "If1" (testStmt "if (1) {}"   "Right (JSIf (JSExpression [JSDecimal \"1\"]) (JSStatementBlock (JSStatementList [])))")

    , testCase "IfElse1" (testStmt "if (1) {} else {}"   "Right (JSIfElse (JSExpression [JSDecimal \"1\"]) (JSStatementBlock (JSStatementList [])) (JSStatementBlock (JSStatementList [])))")
    , testCase "IfElse2" (testStmt "if (1) x=1; else {}" "Right (JSIfElse (JSExpression [JSDecimal \"1\"]) (JSBlock (JSStatementList [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"]])) (JSStatementBlock (JSStatementList [])))")

    , testCase "DoWhile1" (testStmt "do {x=1} while (true);"   "Right (JSDoWhile (JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"]])) (JSExpression [JSLiteral \"true\"]) (JSLiteral \";\"))")
    , testCase "While1"   (testStmt "while(true);"             "Right (JSWhile (JSExpression [JSLiteral \"true\"]) (JSLiteral \";\"))")

    , testCase "For1"   (testStmt "for(;;);"             "Right (JSFor [] [] [] (JSLiteral \";\"))")
    , testCase "For2"   (testStmt "for(x=1;x<10;x++);"   "Right (JSFor [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"]] [JSExpression [JSExpressionBinary \"<\" [JSIdentifier \"x\"] [JSDecimal \"10\"]]] [JSExpression [JSExpressionPostfix \"++\" [JSIdentifier \"x\"]]] (JSLiteral \";\"))")

    , testCase "ForVar1"   (testStmt "for(var x;;);"            "Right (JSForVar [JSVarDecl (JSIdentifier \"x\") []] [] [] (JSLiteral \";\"))")
    , testCase "ForVar2"   (testStmt "for(var x=1;;);"          "Right (JSForVar [JSVarDecl (JSIdentifier \"x\") [JSDecimal \"1\"]] [] [] (JSLiteral \";\"))")
    , testCase "ForVar2"   (testStmt "for(var x;y;z){}"         "Right (JSForVar [JSVarDecl (JSIdentifier \"x\") []] [JSExpression [JSIdentifier \"y\"]] [JSExpression [JSIdentifier \"z\"]] (JSStatementBlock (JSStatementList [])))")

    , testCase "ForIn1"   (testStmt "for(x in 5){}"         "Right (JSForIn [JSIdentifier \"x\"] (JSExpression [JSDecimal \"5\"]) (JSStatementBlock (JSStatementList [])))")

    , testCase "ForVarIn1" (testStmt "for(var x in 5){}"    "Right (JSForVarIn (JSVarDecl (JSIdentifier \"x\") []) (JSExpression [JSDecimal \"5\"]) (JSStatementBlock (JSStatementList [])))")

    , testCase "Var1" (testStmt "var x=1;"        "Right (JSVariables \"var\" [JSVarDecl (JSIdentifier \"x\") [JSDecimal \"1\"]])")
    , testCase "Var2" (testStmt "const x=1,y=2;"  "Right (JSVariables \"const\" [JSVarDecl (JSIdentifier \"x\") [JSDecimal \"1\"],JSVarDecl (JSIdentifier \"y\") [JSDecimal \"2\"]])")

    , testCase "Continue1" (testStmt "continue;"       "Right (JSContinue [JSLiteral \";\"])")
    , testCase "Continue2" (testStmt "continue x;"     "Right (JSContinue [JSIdentifier \"x\",JSLiteral \";\"])")

    , testCase "Break1" (testStmt "break;"       "Right (JSBreak [] [JSLiteral \";\"])")
    , testCase "Break2" (testStmt "break x;"     "Right (JSBreak [JSIdentifier \"x\"] [JSLiteral \";\"])")

    , testCase "Return1" (testStmt "return;"       "Right (JSReturn [JSLiteral \";\"])")
    , testCase "Return2" (testStmt "return x;"     "Right (JSReturn [JSExpression [JSIdentifier \"x\"],JSLiteral \";\"])")

    , testCase "With1" (testStmt "with (x) {};"    "Right (JSWith (JSExpression [JSIdentifier \"x\"]) [JSStatementBlock (JSStatementList []),JSLiteral \";\"])")

    , testCase "Labelled1" (testStmt "abc:x=1"    "Right (JSLabelled (JSIdentifier \"abc\") (JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"]))")

    , testCase "Switch1" (testStmt "switch (x) {}" "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) [JSLiteral \"\"])")
    , testCase "Switch2" (testStmt "switch (x) {case 1:break;}"          "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) [JSCase (JSExpression [JSDecimal \"1\"]) (JSStatementList [JSBreak [] [JSLiteral \";\"]])])")
    , testCase "Switch3" (testStmt "switch (x) {case 0:\ncase 1:break;}"  "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) [JSCase (JSExpression [JSDecimal \"0\"]) (JSStatementList []),JSCase (JSExpression [JSDecimal \"1\"]) (JSStatementList [JSBreak [] [JSLiteral \";\"]])])")
    , testCase "Switch4" (testStmt "switch (x) {default:break;}"          "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) [JSLiteral \"\",JSDefault (JSStatementList [JSBreak [] [JSLiteral \";\"]]),JSLiteral \"\"])")
    , testCase "Switch5" (testStmt "switch (x) {default:\ncase 1:break;}"   "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) [JSLiteral \"\",JSDefault (JSStatementList []),JSCase (JSExpression [JSDecimal \"1\"]) (JSStatementList [JSBreak [] [JSLiteral \";\"]])])")

    , testCase "Throw1" (testStmt "throw 1"   "Right (JSThrow (JSExpression [JSDecimal \"1\"]))")

    , testCase "Try1" (testStmt "try{}catch(a){}"            "Right (JSTry (JSBlock (JSStatementList [])) [JSCatch (JSIdentifier \"a\") [] (JSBlock (JSStatementList []))])")
    , testCase "Try2" (testStmt "try{}finally{}"             "Right (JSTry (JSBlock (JSStatementList [])) [JSFinally (JSBlock (JSStatementList []))])")
    , testCase "Try3" (testStmt "try{}catch(a){}finally{}"   "Right (JSTry (JSBlock (JSStatementList [])) [JSCatch (JSIdentifier \"a\") [] (JSBlock (JSStatementList [])),JSFinally (JSBlock (JSStatementList []))])")

    , testCase "Try4" (testStmt "try{}catch(a){}catch(b){}finally{}"   "Right (JSTry (JSBlock (JSStatementList [])) [JSCatch (JSIdentifier \"a\") [] (JSBlock (JSStatementList [])),JSCatch (JSIdentifier \"b\") [] (JSBlock (JSStatementList [])),JSFinally (JSBlock (JSStatementList []))])")
    , testCase "Try5" (testStmt "try{}catch(a){}catch(b){}"            "Right (JSTry (JSBlock (JSStatementList [])) [JSCatch (JSIdentifier \"a\") [] (JSBlock (JSStatementList [])),JSCatch (JSIdentifier \"b\") [] (JSBlock (JSStatementList []))])")
    , testCase "Try6" (testStmt "try{}catch(a if true){}catch(b){}"     "Right (JSTry (JSBlock (JSStatementList [])) [JSCatch (JSIdentifier \"a\") [JSLiteral \"true\"] (JSBlock (JSStatementList [])),JSCatch (JSIdentifier \"b\") [] (JSBlock (JSStatementList []))])")

    , testCase "Function1" (testProg "function a(){}"      "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"a\") [] (JSFunctionBody [])])")
    , testCase "Function2" (testProg "function a(b,c){}"   "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"a\") [JSIdentifier \"b\",JSIdentifier \"c\"] (JSFunctionBody [])])")

    , testCase "Comment1" (testProg "//blah\nx=1;//foo\na"   "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\",JSExpression [JSIdentifier \"a\"]])")

    , testCase "Comment2" (testProg "/*x=1\ny=2\n*/z=2;//foo\na"   "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"z\",JSOperator \"=\",JSDecimal \"2\"],JSLiteral \";\",JSExpression [JSIdentifier \"a\"]])")

    , testCase "min_100_animals1" (testProg "function Animal(name){if(!name)throw new Error('Must specify an animal name');this.name=name};Animal.prototype.toString=function(){return this.name};o=new Animal(\"bob\");o.toString()==\"bob\"" "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"Animal\") [JSIdentifier \"name\"] (JSFunctionBody [JSSourceElements [JSIf (JSExpression [JSUnary \"!\",JSIdentifier \"name\"]) (JSBlock (JSStatementList [JSThrow (JSExpression [JSLiteral \"new \",JSIdentifier \"Error\",JSArguments [[JSStringLiteral '\\'' \"Must specify an animal name\"]]])])),JSExpression [JSMemberDot [JSLiteral \"this\"] (JSIdentifier \"name\"),JSOperator \"=\",JSIdentifier \"name\"]]]),JSLiteral \";\",JSExpression [JSMemberDot [JSMemberDot [JSIdentifier \"Animal\"] (JSIdentifier \"prototype\")] (JSIdentifier \"toString\"),JSOperator \"=\",JSFunctionExpression [] [] (JSFunctionBody [JSSourceElements [JSReturn [JSExpression [JSMemberDot [JSLiteral \"this\"] (JSIdentifier \"name\")],JSLiteral \"\"]]])],JSLiteral \";\",JSExpression [JSIdentifier \"o\",JSOperator \"=\",JSLiteral \"new \",JSIdentifier \"Animal\",JSArguments [[JSStringLiteral '\"' \"bob\"]]],JSLiteral \";\",JSExpression [JSExpressionBinary \"==\" [JSMemberDot [JSIdentifier \"o\"] (JSIdentifier \"toString\"),JSArguments []] [JSStringLiteral '\"' \"bob\"]]])")

    , testCase "min_100_animals2" (testProg "Animal=function(){return this.name};" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"Animal\",JSOperator \"=\",JSFunctionExpression [] [] (JSFunctionBody [JSSourceElements [JSReturn [JSExpression [JSMemberDot [JSLiteral \"this\"] (JSIdentifier \"name\")],JSLiteral \"\"]]])],JSLiteral \";\"])")

    , testCase "min_100_animals3" (testProg "if(a)x=1;y=2" "Right (JSSourceElementsTop [JSIf (JSExpression [JSIdentifier \"a\"]) (JSBlock (JSStatementList [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"]])),JSExpression [JSIdentifier \"y\",JSOperator \"=\",JSDecimal \"2\"]])")

    , testCase "min_100_animals4" (testProg "if(a)x=a()y=2" "Right (JSSourceElementsTop [JSIf (JSExpression [JSIdentifier \"a\"]) (JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSIdentifier \"a\",JSArguments []]),JSExpression [JSIdentifier \"y\",JSOperator \"=\",JSDecimal \"2\"]])")

    , testCase "05_regex"  (testProg "newlines=spaces.match(/\\n/g)" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"newlines\",JSOperator \"=\",JSMemberDot [JSIdentifier \"spaces\"] (JSIdentifier \"match\"),JSArguments [[JSRegEx \"/\\\\n/g\"]]]])")

    , testCase "05_regex2" (testProg "x=/\\n/g" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSRegEx \"/\\\\n/g\"]])")

    , testCase "05_regex3" (testProg "x=i(/[?|^&(){}\\[\\]+\\-*\\/\\.]/g,\"\\\\$&\")" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSIdentifier \"i\",JSArguments [[JSRegEx \"/[?|^&(){}\\\\[\\\\]+\\\\-*\\\\/\\\\.]/g\"],[JSStringLiteral '\"' \"\\\\\\\\$&\"]]]])")

    , testCase "05_regex4" (testProg "x=i(/^$/g,\"\\\\$&\")" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSIdentifier \"i\",JSArguments [[JSRegEx \"/^$/g\"],[JSStringLiteral '\"' \"\\\\\\\\$&\"]]]])")

   , testCase "05_regex5" (testProg "if(/^[a-z]/.test(t)){consts+=t.toUpperCase();keywords[t]=i}else consts+=(/^\\W/.test(t)?opTypeNames[t]:t);" "Right (JSSourceElementsTop [JSIfElse (JSExpression [JSMemberDot [JSRegEx \"/^[a-z]/\"] (JSIdentifier \"test\"),JSArguments [[JSIdentifier \"t\"]]]) (JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"consts\",JSOperator \"+=\",JSMemberDot [JSIdentifier \"t\"] (JSIdentifier \"toUpperCase\"),JSArguments []],JSLiteral \";\",JSExpression [JSMemberSquare [JSIdentifier \"keywords\"] (JSExpression [JSIdentifier \"t\"]),JSOperator \"=\",JSIdentifier \"i\"]])) (JSExpression [JSIdentifier \"consts\",JSOperator \"+=\",JSExpressionParen (JSExpression [JSExpressionTernary [JSMemberDot [JSRegEx \"/^\\\\W/\"] (JSIdentifier \"test\"),JSArguments [[JSIdentifier \"t\"]]] [JSMemberSquare [JSIdentifier \"opTypeNames\"] (JSExpression [JSIdentifier \"t\"])] [JSIdentifier \"t\"]])]),JSLiteral \";\"])")

   , testCase "if_semi" (testProg "if(x);x=1" "Right (JSSourceElementsTop [JSIf (JSExpression [JSIdentifier \"x\"]) (JSLiteral \";\"),JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"]])")

   , testCase "67_bob" (testProg "(match = /^\"(?:\\\\.|[^\"])*\"|^'(?:[^']|\\\\.)*'/(input))" "Right (JSSourceElementsTop [JSExpression [JSExpressionParen (JSExpression [JSIdentifier \"match\",JSOperator \"=\",JSRegEx \"/^\\\"(?:\\\\\\\\.|[^\\\"])*\\\"|^'(?:[^']|\\\\\\\\.)*'/\",JSArguments [[JSIdentifier \"input\"]]])]])")

   , testCase "122_jsexec" (testProg "v = getValue(execute(n[0], x)) in getValue(execute(n[1], x));" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"v\",JSOperator \"=\",JSExpressionBinary \" in \" [JSIdentifier \"getValue\",JSArguments [[JSIdentifier \"execute\",JSArguments [[JSMemberSquare [JSIdentifier \"n\"] (JSExpression [JSDecimal \"0\"])],[JSIdentifier \"x\"]]]]] [JSIdentifier \"getValue\",JSArguments [[JSIdentifier \"execute\",JSArguments [[JSMemberSquare [JSIdentifier \"n\"] (JSExpression [JSDecimal \"1\"])],[JSIdentifier \"x\"]]]]]],JSLiteral \";\"])")

   , testCase "bug1" (testProg "/* */\nfunction f() {\n/*  */\n}\n" "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"f\") [] (JSFunctionBody [])])")
   , testCase "bug1" (testProg "/* **/\nfunction f() {\n/*  */\n}\n" "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"f\") [] (JSFunctionBody [])])")

   , testCase "unicode1-ws" (testProg "a \f\v\t\r\n=\x00a0\x1680\x180e\x2000\x2001\x2002\x2003\x2004\x2005\x2006\x2007\x2008\x2009\x200a\x2028\x2029\x202f\x205f\x3000\&1;" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"a\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\"])")

   , testCase "unicode2-lt" (testProg "//comment\x000Ax=1;" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\"])")
   , testCase "unicode3-lt" (testProg "//comment\x000Dx=1;" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\"])")
   , testCase "unicode4-lt" (testProg "//comment\x2028x=1;" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\"])")
   , testCase "unicode5-lt" (testProg "//comment\x2029x=1;" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\"])")

   , testCase "unicode2" (testProg "àáâãäå = 1;" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"\\224\\225\\226\\227\\228\\229\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\"])")

   , testCase "unicode3" (testProg "$aà = 1;_b=2;\0065a=2"  "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"$a\\224\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\",JSExpression [JSIdentifier \"_b\",JSOperator \"=\",JSDecimal \"2\"],JSLiteral \";\",JSExpression [JSIdentifier \"Aa\",JSOperator \"=\",JSDecimal \"2\"]])")

   , testCase "unicode4" (testProg "x=\"àáâãäå\";y='\3012a\0068'" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSStringLiteral '\"' \"\\224\\225\\226\\227\\228\\229\"],JSLiteral \";\",JSExpression [JSIdentifier \"y\",JSOperator \"=\",JSStringLiteral '\\'' \"\\3012aD\"]])")

   , testCase "unicode5" (testFile "./test/Unicode.js" "JSSourceElementsTop [JSExpression [JSIdentifier \"\\224\\225\\226\\227\\228\\229\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\"]")

   , testCase "bug2.a" (testProg "function() {\nz = function /*z*/(o) {\nreturn r;\n};}" "Right (JSSourceElementsTop [JSExpression [JSFunctionExpression [] [] (JSFunctionBody [JSSourceElements [JSExpression [JSIdentifier \"z\",JSOperator \"=\",JSFunctionExpression [] [JSIdentifier \"o\"] (JSFunctionBody [JSSourceElements [JSReturn [JSExpression [JSIdentifier \"r\"],JSLiteral \";\"]]])],JSLiteral \";\"]])]])")

   , testCase "bug2.b" (testProg "function() {\nz = function z(o) {\nreturn r;\n};}" "Right (JSSourceElementsTop [JSExpression [JSFunctionExpression [] [] (JSFunctionBody [JSSourceElements [JSExpression [JSIdentifier \"z\",JSOperator \"=\",JSFunctionExpression [JSIdentifier \"z\"] [JSIdentifier \"o\"] (JSFunctionBody [JSSourceElements [JSReturn [JSExpression [JSIdentifier \"r\"],JSLiteral \";\"]]])],JSLiteral \";\"]])]])")

   -- https://github.com/alanz/hjsmin/issues/#issue/3
   , testCase "bug3" (testProg "var myLatlng = new google.maps.LatLng(56.8379100, 60.5806664);" "Right (JSSourceElementsTop [JSVariables \"var\" [JSVarDecl (JSIdentifier \"myLatlng\") [JSLiteral \"new \",JSMemberDot [JSMemberDot [JSIdentifier \"google\"] (JSIdentifier \"maps\")] (JSIdentifier \"LatLng\"),JSArguments [[JSDecimal \"56.8379100\"],[JSDecimal \"60.5806664\"]]]]])")

   -- https://github.com/alanz/hjsmin/issues/#issue/4
   , testCase "bug4" (testProg "/* * geolocation. пытаемся определить свое местоположение * если не получается то используем defaultLocation * @Param {object} map экземпляр карты * @Param {object LatLng} defaultLocation Координаты центра по умолчанию * @Param {function} callbackAfterLocation Фу-ия которая вызывается после * геолокации. Т.к запрос геолокации асинхронен */x" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\"]])")



   , testCase "02_sm.js"   (testProg "{zero}\none1;two\n{three\nfour;five;\n{\nsix;{seven;}\n}\n}" "Right (JSSourceElementsTop [JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"zero\"]]),JSExpression [JSIdentifier \"one1\"],JSLiteral \";\",JSExpression [JSIdentifier \"two\"],JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"three\"],JSExpression [JSIdentifier \"four\"],JSLiteral \";\",JSExpression [JSIdentifier \"five\"],JSLiteral \";\",JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"six\"],JSLiteral \";\",JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"seven\"],JSLiteral \";\"])])])])")

   , testCase "02_sm.js.2" (testProg "{zero}\nget;two\n{three\nfour;set;\n{\nsix;{seven;}\n}\n}" "Right (JSSourceElementsTop [JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"zero\"]]),JSExpression [JSIdentifier \"get\"],JSLiteral \";\",JSExpression [JSIdentifier \"two\"],JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"three\"],JSExpression [JSIdentifier \"four\"],JSLiteral \";\",JSExpression [JSIdentifier \"set\"],JSLiteral \";\",JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"six\"],JSLiteral \";\",JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"seven\"],JSLiteral \";\"])])])])")

   , testCase "loc1" (testProgUn "x = 1\n  y=2;" "Right (NS (JSSourceElementsTop [NS (JSExpression [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [NoComment],NS (JSOperator \"=\") (TokenPn 2 1 3) [NoComment],NS (JSDecimal \"1\") (TokenPn 4 1 5) [NoComment]]) (TokenPn 0 1 1) [],NS (JSExpression [NS (JSIdentifier \"y\") (TokenPn 8 2 3) [NoComment],NS (JSOperator \"=\") (TokenPn 9 2 4) [NoComment],NS (JSDecimal \"2\") (TokenPn 10 2 5) [NoComment]]) (TokenPn 8 2 3) [],NS (JSLiteral \";\") (TokenPn 11 2 6) [NoComment]]) (TokenPn 0 1 1) [])")

   -- https://github.com/alanz/language-javascript/issues/2
   , testCase "issue2" (testProg "var img = document.createElement('img');\nimg.src = \"mylogo.jpg\";\n$(img).click(function() {\n   alert('clicked!');\n});" "Right (JSSourceElementsTop [JSVariables \"var\" [JSVarDecl (JSIdentifier \"img\") [JSMemberDot [JSIdentifier \"document\"] (JSIdentifier \"createElement\"),JSArguments [[JSStringLiteral '\\'' \"img\"]]]],JSExpression [JSMemberDot [JSIdentifier \"img\"] (JSIdentifier \"src\"),JSOperator \"=\",JSStringLiteral '\"' \"mylogo.jpg\"],JSLiteral \";\",JSExpression [JSIdentifier \"$\",JSArguments [[JSIdentifier \"img\"]],JSCallExpression \".\" [JSIdentifier \"click\"],JSCallExpression \"()\" [JSArguments [[JSFunctionExpression [] [] (JSFunctionBody [JSSourceElements [JSExpression [JSIdentifier \"alert\",JSArguments [[JSStringLiteral '\\'' \"clicked!\"]]],JSLiteral \";\"]])]]]],JSLiteral \";\"])")


   -- Working in ECMASCRIPT 5.1 changes
   , testCase "lineTerminatorInString1" (testProg "x='abc\\\ndef';"    "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSStringLiteral '\\'' \"abcdef\"],JSLiteral \";\"])")
   , testCase "lineTerminatorInString2" (testProg "x=\"abc\\\ndef\";" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSStringLiteral '\"' \"abcdef\"],JSLiteral \";\"])")
   , testCase "lineTerminatorInString3" (testProg "x=\"abc\\\rdef\";" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSStringLiteral '\"' \"abcdef\"],JSLiteral \";\"])")
   , testCase "lineTerminatorInString4" (testProg "x=\"abc\\\x2028 def\";" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSStringLiteral '\"' \"abc def\"],JSLiteral \";\"])")
   , testCase "lineTerminatorInString5" (testProg "x=\"abc\\\x2029 def\";" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSStringLiteral '\"' \"abc def\"],JSLiteral \";\"])")
   , testCase "lineTerminatorInString6" (testProg "x=\"abc\\\r\ndef\";" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSStringLiteral '\"' \"abcdef\"],JSLiteral \";\"])")


     -- https://github.com/alanz/language-javascript/issues/4
   , testCase "issue4ok"   (testProg "var k = {\ny: somename\n}" "Right (JSSourceElementsTop [JSVariables \"var\" [JSVarDecl (JSIdentifier \"k\") [JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"y\") [JSIdentifier \"somename\"]]]]])")
   , testCase "issue4bug1" (testProg "var k = {\ny: code\n}" "Right (JSSourceElementsTop [JSVariables \"var\" [JSVarDecl (JSIdentifier \"k\") [JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"y\") [JSIdentifier \"code\"]]]]])")
   , testCase "issue4bug2" (testProg "var k = {\ny: mode\n}" "Right (JSSourceElementsTop [JSVariables \"var\" [JSVarDecl (JSIdentifier \"k\") [JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"y\") [JSIdentifier \"mode\"]]]]])")

     -- https://github.com/alanz/language-javascript/issues/5
   , testCase "issue5bug1" (testProg "x = { y: 1e8 }" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"y\") [JSDecimal \"1e8\"]]]])")
   , testCase "issue5ok2" (testProg "{ y: 1e8 }" "Right (JSSourceElementsTop [JSStatementBlock (JSStatementList [JSLabelled (JSIdentifier \"y\") (JSExpression [JSDecimal \"1e8\"])])])")
   , testCase "issue5ok3" (testProg "{ y: 18 }" "Right (JSSourceElementsTop [JSStatementBlock (JSStatementList [JSLabelled (JSIdentifier \"y\") (JSExpression [JSDecimal \"18\"])])])")
   , testCase "issue5ok4" (testProg "x = { y: 18 }" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"y\") [JSDecimal \"18\"]]]])")
 -}
    ]

-- ---------------------------------------------------------------------

commentPrintSuite :: Test
commentPrintSuite = testGroup "Comments"
    [
      testCase "LiteralNull"       (testRoundTrip "/*a*/null")
    , testCase "LiteralFalse"      (testRoundTrip "/*b*/false")
    , testCase "LiteralTrue"       (testRoundTrip "true")
    , testCase "LiteralTrue"       (testRoundTrip "/*c*/true")

    , testCase "LiteralHexInteger" (testRoundTrip "/*d*/0x1234fF")
    , testCase "LiteralDecimal"    (testRoundTrip "/*e*/1.0e4")
    , testCase "LiteralString1"    (testRoundTrip "/*f*/\"hello\\nworld\"")
    , testCase "LiteralString2"    (testRoundTrip "/*g*/'hello\\nworld'")


    , testCase "LiteralThis"       (testRoundTrip "/*h*/this")

    , testCase "LiteralRegex1"     (testRoundTrip "/*i*//blah/")

    , testCase "Identifier2"       (testRoundTrip "//j\nthis_")

    , testCase "ArrayLiteral1"     (testRoundTrip "/*a*/[/*b*/]")
    , testCase "ArrayLiteral2"     (testRoundTrip "/*a*/[/*b*/,/*c*/]")
    , testCase "ArrayLiteral3"     (testRoundTrip "/*a*/[/*b*/,/*c*/,/*d*/]")
    , testCase "ArrayLiteral4"     (testRoundTrip "/*a*/[/*b/*,/*c*/,/*d*/x/*e*/]")
    , testCase "ArrayLiteral5"     (testRoundTrip "/*a*/[/*b*/,/*c*/,/*d*/x/*e*/]")
    , testCase "ArrayLiteral6"     (testRoundTrip "/*a*/[/*b*/,/*c*/x/*d*/,/*e*/,/*f*/x/*g*/]")
    , testCase "ArrayLiteral7"     (testRoundTrip "/*a*/[/*b*/x/*c*/]")
    , testCase "ArrayLiteral8"     (testRoundTrip "/*a*/[/*b*/x/*c*/,/*d*/]")

    , testCase "ObjectLiteral1"    (testRoundTrip "/*a*/{/*b*/}")
    , testCase "ObjectLiteral2"    (testRoundTrip "/*a*/{/*b*/x/*c*/:/*d*/1/*e*/}")
    , testCase "ObjectLiteral3"    (testRoundTrip "/*a*/{/*b*/x/*c*/:/*d*/1/*e*/,/*f*/y/*g*/:/*h*/2/*i*/}")

    , testCase "ObjectLiteral5"    (testRoundTrip "/*a*/{/*b*/x/*c*/:/*d*/1/*e*/,/*f*/}")

    -- Edition 5 extensions
    , testCase "ObjectLiteral7"    (testRoundTrip "/*a*/x/*b*/=/*c*/{/*d*/get/*e*/ foo/*f*/(/*g*/)/*h*/ {/*i*/return/*j*/ 1/*k*/}/*l*/,/*m*/set/*n*/ foo/*o*/(/*p*/a/*q*/) /*r*/{/*s*/x/*t*/=/*u*/a/*v*/}/*w*/}")

    , testCase "ExpressionParen"   (testRoundTrip "/*a*/(/*b*/56/*c*/)")

    , testCase "Statement1"        (testRoundTrip "/*a*/x")

    , testCase "Statement2"        (testRoundTrip "/*a*/null")

    , testCase "Statement3"        (testRoundTrip "/*a*/true/*b*/?/*c*/1/*d*/:/*e*/2")

    , testCase "Statement4"        (testRoundTrip "/*a*/x/*b*/||/*c*/y")

    , testCase "Statement5"        (testRoundTrip "/*a*/x/*b*/&&/*c*/y")

    , testCase "Statement6a"       (testRoundTrip "/*a*/x/*b*/|/*c*/y")

    , testCase "Statement6b"       (testRoundTrip "/*a*/x/*b*/^/*c*/y")

    , testCase "Statement7"        (testRoundTrip "/*a*/x/*b*/&/*c*/y")

    , testCase "Statement8"        (testRoundTrip "/*a*/x/*b*/==/*c*/y")

    , testCase "Statement9"        (testRoundTrip "/*a*/x/*b*/!=/*c*/y")

    , testCase "Statement10"       (testRoundTrip "/*a*/x/*b*/===/*c*/y")

    , testCase "Statement11"       (testRoundTrip "/*a*/x/*b*/!==/*c*/y")

    , testCase "Statement12a"       (testRoundTrip "/*a*/x/*b*/</*c*/y")

    , testCase "Statement12b"       (testRoundTrip "/*a*/x/*b*/>/*c*/y")

    , testCase "Statement12c"       (testRoundTrip "/*a*/x/*b*/<=/*c*/y")

    , testCase "Statement12d"       (testRoundTrip "/*a*/x/*b*/>=/*c*/y")

    , testCase "Statement12e"       (testRoundTrip "/*a*/x /*b*/instanceof /*c*/y")

      {-


    , testCase "Statement13"       (testStmt "x<<y"     "Right (JSExpression [JSExpressionBinary \"<<\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement13"       (testStmt "x>>y"     "Right (JSExpression [JSExpressionBinary \">>\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement13"       (testStmt "x>>>y"     "Right (JSExpression [JSExpressionBinary \">>>\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")

    , testCase "Statement14"       (testStmt "x+y"     "Right (JSExpression [JSExpressionBinary \"+\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement14"       (testStmt "x-y"     "Right (JSExpression [JSExpressionBinary \"-\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")

    , testCase "Statement15"       (testStmt "x*y"     "Right (JSExpression [JSExpressionBinary \"*\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement16"       (testStmt "x/y"     "Right (JSExpression [JSExpressionBinary \"/\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement17"       (testStmt "x%y"     "Right (JSExpression [JSExpressionBinary \"%\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")

    , testCase "Statement18"       (testStmt "delete y"  "Right (JSExpression [JSUnary \"delete \",JSIdentifier \"y\"])")
    , testCase "Statement19"       (testStmt "void y"    "Right (JSExpression [JSUnary \"void \",JSIdentifier \"y\"])")
    , testCase "Statement20"       (testStmt "typeof y"  "Right (JSExpression [JSUnary \"typeof \",JSIdentifier \"y\"])")
    , testCase "Statement21"       (testStmt "++y"    "Right (JSExpression [JSUnary \"++\",JSIdentifier \"y\"])")
    , testCase "Statement22"       (testStmt "--y"    "Right (JSExpression [JSUnary \"--\",JSIdentifier \"y\"])")
    , testCase "Statement23"       (testStmt "+y"     "Right (JSExpression [JSUnary \"+\",JSIdentifier \"y\"])")
    , testCase "Statement24"       (testStmt "-y"     "Right (JSExpression [JSUnary \"-\",JSIdentifier \"y\"])")
    , testCase "Statement25"       (testStmt "~y"     "Right (JSExpression [JSUnary \"~\",JSIdentifier \"y\"])")
    , testCase "Statement26"       (testStmt "!y"     "Right (JSExpression [JSUnary \"!\",JSIdentifier \"y\"])")

    , testCase "Statement27"       (testStmt "y++"     "Right (JSExpression [JSExpressionPostfix \"++\" [JSIdentifier \"y\"]])")
    , testCase "Statement28"       (testStmt "y--"     "Right (JSExpression [JSExpressionPostfix \"--\" [JSIdentifier \"y\"]])")

      -- Member Expressions
    , testCase "MemberExpression1" (testStmt "function(){}"    "Right (JSExpression [JSFunctionExpression [] [] (JSFunctionBody [])])")
    , testCase "MemberExpression1" (testStmt "function(a){}"    "Right (JSExpression [JSFunctionExpression [] [JSIdentifier \"a\"] (JSFunctionBody [])])")
    , testCase "MemberExpression1" (testStmt "function(a,b){}"  "Right (JSExpression [JSFunctionExpression [] [JSIdentifier \"a\",JSIdentifier \"b\"] (JSFunctionBody [])])")

    , testCase "MemberExpression1" (testStmt "x[y]"     "Right (JSExpression [JSMemberSquare [JSIdentifier \"x\"] (JSExpression [JSIdentifier \"y\"])])")
    , testCase "MemberExpression1" (testStmt "x[y][z]"  "Right (JSExpression [JSMemberSquare [JSMemberSquare [JSIdentifier \"x\"] (JSExpression [JSIdentifier \"y\"])] (JSExpression [JSIdentifier \"z\"])])")
    , testCase "MemberExpression1" (testStmt "x.y"      "Right (JSExpression [JSMemberDot [JSIdentifier \"x\"] (JSIdentifier \"y\")])")
    , testCase "MemberExpression1" (testStmt "x.y.z"    "Right (JSExpression [JSMemberDot [JSMemberDot [JSIdentifier \"x\"] (JSIdentifier \"y\")] (JSIdentifier \"z\")])")

    , testCase "MemberExpression1" (testStmt "new x()"  "Right (JSExpression [JSLiteral \"new \",JSIdentifier \"x\",JSArguments []])")

    , testCase "NewExpression1" (testStmt "new x.y"  "Right (JSExpression [JSLiteral \"new \",JSMemberDot [JSIdentifier \"x\"] (JSIdentifier \"y\")])")

    , testCase "CallExpression1" (testStmt "x()"     "Right (JSExpression [JSIdentifier \"x\",JSArguments []])")
    , testCase "CallExpression2" (testStmt "x()()"   "Right (JSExpression [JSIdentifier \"x\",JSArguments [],JSCallExpression \"()\" [JSArguments []]])")
    , testCase "CallExpression3" (testStmt "x()[4]"  "Right (JSExpression [JSIdentifier \"x\",JSArguments [],JSCallExpression \"[]\" [JSExpression [JSDecimal \"4\"]]])")
    , testCase "CallExpression4" (testStmt "x().x"   "Right (JSExpression [JSIdentifier \"x\",JSArguments [],JSCallExpression \".\" [JSIdentifier \"x\"]])")
    , testCase "CallExpression5" (testStmt "x(a,b=2).x"   "Right (JSExpression [JSIdentifier \"x\",JSArguments [[JSIdentifier \"a\"],[JSIdentifier \"b\",JSOperator \"=\",JSDecimal \"2\"]],JSCallExpression \".\" [JSIdentifier \"x\"]])")

    , testCase "AssignExpression1" (testStmt "x=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x*=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"*=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x/=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"/=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x%=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"%=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x+=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"+=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x-=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"-=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x<<=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"<<=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x>>=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \">>=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x>>>=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \">>>=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x&=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"&=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x^=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"^=\",JSDecimal \"1\"])")
    , testCase "AssignExpression1" (testStmt "x|=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator \"|=\",JSDecimal \"1\"])")


    , testCase "Block1" (testStmt "{}"     "Right (JSStatementBlock (JSStatementList []))")
    , testCase "Block2" (testStmt "{x=1}"  "Right (JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"]]))")
    , testCase "Block3" (testStmt "{x=1;y=2}"   "Right (JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\",JSExpression [JSIdentifier \"y\",JSOperator \"=\",JSDecimal \"2\"]]))")
    , testCase "Block4" (testStmt "{{}}"     "Right (JSStatementBlock (JSStatementList [JSStatementBlock (JSStatementList [])]))")
    , testCase "Block5" (testStmt "{{{}}}"   "Right (JSStatementBlock (JSStatementList [JSStatementBlock (JSStatementList [JSStatementBlock (JSStatementList [])])]))")

    , testCase "If1" (testStmt "if (1) {}"   "Right (JSIf (JSExpression [JSDecimal \"1\"]) (JSStatementBlock (JSStatementList [])))")

    , testCase "IfElse1" (testStmt "if (1) {} else {}"   "Right (JSIfElse (JSExpression [JSDecimal \"1\"]) (JSStatementBlock (JSStatementList [])) (JSStatementBlock (JSStatementList [])))")
    , testCase "IfElse2" (testStmt "if (1) x=1; else {}" "Right (JSIfElse (JSExpression [JSDecimal \"1\"]) (JSBlock (JSStatementList [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"]])) (JSStatementBlock (JSStatementList [])))")

    , testCase "DoWhile1" (testStmt "do {x=1} while (true);"   "Right (JSDoWhile (JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"]])) (JSExpression [JSLiteral \"true\"]) (JSLiteral \";\"))")
    , testCase "While1"   (testStmt "while(true);"             "Right (JSWhile (JSExpression [JSLiteral \"true\"]) (JSLiteral \";\"))")

    , testCase "For1"   (testStmt "for(;;);"             "Right (JSFor [] [] [] (JSLiteral \";\"))")
    , testCase "For2"   (testStmt "for(x=1;x<10;x++);"   "Right (JSFor [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"]] [JSExpression [JSExpressionBinary \"<\" [JSIdentifier \"x\"] [JSDecimal \"10\"]]] [JSExpression [JSExpressionPostfix \"++\" [JSIdentifier \"x\"]]] (JSLiteral \";\"))")

    , testCase "ForVar1"   (testStmt "for(var x;;);"            "Right (JSForVar [JSVarDecl (JSIdentifier \"x\") []] [] [] (JSLiteral \";\"))")
    , testCase "ForVar2"   (testStmt "for(var x=1;;);"          "Right (JSForVar [JSVarDecl (JSIdentifier \"x\") [JSDecimal \"1\"]] [] [] (JSLiteral \";\"))")
    , testCase "ForVar2"   (testStmt "for(var x;y;z){}"         "Right (JSForVar [JSVarDecl (JSIdentifier \"x\") []] [JSExpression [JSIdentifier \"y\"]] [JSExpression [JSIdentifier \"z\"]] (JSStatementBlock (JSStatementList [])))")

    , testCase "ForIn1"   (testStmt "for(x in 5){}"         "Right (JSForIn [JSIdentifier \"x\"] (JSExpression [JSDecimal \"5\"]) (JSStatementBlock (JSStatementList [])))")

    , testCase "ForVarIn1" (testStmt "for(var x in 5){}"    "Right (JSForVarIn (JSVarDecl (JSIdentifier \"x\") []) (JSExpression [JSDecimal \"5\"]) (JSStatementBlock (JSStatementList [])))")

    , testCase "Var1" (testStmt "var x=1;"        "Right (JSVariables \"var\" [JSVarDecl (JSIdentifier \"x\") [JSDecimal \"1\"]])")
    , testCase "Var2" (testStmt "const x=1,y=2;"  "Right (JSVariables \"const\" [JSVarDecl (JSIdentifier \"x\") [JSDecimal \"1\"],JSVarDecl (JSIdentifier \"y\") [JSDecimal \"2\"]])")

    , testCase "Continue1" (testStmt "continue;"       "Right (JSContinue [JSLiteral \";\"])")
    , testCase "Continue2" (testStmt "continue x;"     "Right (JSContinue [JSIdentifier \"x\",JSLiteral \";\"])")

    , testCase "Break1" (testStmt "break;"       "Right (JSBreak [] [JSLiteral \";\"])")
    , testCase "Break2" (testStmt "break x;"     "Right (JSBreak [JSIdentifier \"x\"] [JSLiteral \";\"])")

    , testCase "Return1" (testStmt "return;"       "Right (JSReturn [JSLiteral \";\"])")
    , testCase "Return2" (testStmt "return x;"     "Right (JSReturn [JSExpression [JSIdentifier \"x\"],JSLiteral \";\"])")

    , testCase "With1" (testStmt "with (x) {};"    "Right (JSWith (JSExpression [JSIdentifier \"x\"]) [JSStatementBlock (JSStatementList []),JSLiteral \";\"])")

    , testCase "Labelled1" (testStmt "abc:x=1"    "Right (JSLabelled (JSIdentifier \"abc\") (JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"]))")

    , testCase "Switch1" (testStmt "switch (x) {}" "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) [JSLiteral \"\"])")
    , testCase "Switch2" (testStmt "switch (x) {case 1:break;}"          "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) [JSCase (JSExpression [JSDecimal \"1\"]) (JSStatementList [JSBreak [] [JSLiteral \";\"]])])")
    , testCase "Switch3" (testStmt "switch (x) {case 0:\ncase 1:break;}"  "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) [JSCase (JSExpression [JSDecimal \"0\"]) (JSStatementList []),JSCase (JSExpression [JSDecimal \"1\"]) (JSStatementList [JSBreak [] [JSLiteral \";\"]])])")
    , testCase "Switch4" (testStmt "switch (x) {default:break;}"          "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) [JSLiteral \"\",JSDefault (JSStatementList [JSBreak [] [JSLiteral \";\"]]),JSLiteral \"\"])")
    , testCase "Switch5" (testStmt "switch (x) {default:\ncase 1:break;}"   "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) [JSLiteral \"\",JSDefault (JSStatementList []),JSCase (JSExpression [JSDecimal \"1\"]) (JSStatementList [JSBreak [] [JSLiteral \";\"]])])")

    , testCase "Throw1" (testStmt "throw 1"   "Right (JSThrow (JSExpression [JSDecimal \"1\"]))")

    , testCase "Try1" (testStmt "try{}catch(a){}"            "Right (JSTry (JSBlock (JSStatementList [])) [JSCatch (JSIdentifier \"a\") [] (JSBlock (JSStatementList []))])")
    , testCase "Try2" (testStmt "try{}finally{}"             "Right (JSTry (JSBlock (JSStatementList [])) [JSFinally (JSBlock (JSStatementList []))])")
    , testCase "Try3" (testStmt "try{}catch(a){}finally{}"   "Right (JSTry (JSBlock (JSStatementList [])) [JSCatch (JSIdentifier \"a\") [] (JSBlock (JSStatementList [])),JSFinally (JSBlock (JSStatementList []))])")

    , testCase "Try4" (testStmt "try{}catch(a){}catch(b){}finally{}"   "Right (JSTry (JSBlock (JSStatementList [])) [JSCatch (JSIdentifier \"a\") [] (JSBlock (JSStatementList [])),JSCatch (JSIdentifier \"b\") [] (JSBlock (JSStatementList [])),JSFinally (JSBlock (JSStatementList []))])")
    , testCase "Try5" (testStmt "try{}catch(a){}catch(b){}"            "Right (JSTry (JSBlock (JSStatementList [])) [JSCatch (JSIdentifier \"a\") [] (JSBlock (JSStatementList [])),JSCatch (JSIdentifier \"b\") [] (JSBlock (JSStatementList []))])")
    , testCase "Try6" (testStmt "try{}catch(a if true){}catch(b){}"     "Right (JSTry (JSBlock (JSStatementList [])) [JSCatch (JSIdentifier \"a\") [JSLiteral \"true\"] (JSBlock (JSStatementList [])),JSCatch (JSIdentifier \"b\") [] (JSBlock (JSStatementList []))])")

    , testCase "Function1" (testProg "function a(){}"      "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"a\") [] (JSFunctionBody [])])")
    , testCase "Function2" (testProg "function a(b,c){}"   "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"a\") [JSIdentifier \"b\",JSIdentifier \"c\"] (JSFunctionBody [])])")

    , testCase "Comment1" (testProg "//blah\nx=1;//foo\na"   "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\",JSExpression [JSIdentifier \"a\"]])")

    , testCase "Comment2" (testProg "/*x=1\ny=2\n*/z=2;//foo\na"   "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"z\",JSOperator \"=\",JSDecimal \"2\"],JSLiteral \";\",JSExpression [JSIdentifier \"a\"]])")

    , testCase "min_100_animals1" (testProg "function Animal(name){if(!name)throw new Error('Must specify an animal name');this.name=name};Animal.prototype.toString=function(){return this.name};o=new Animal(\"bob\");o.toString()==\"bob\"" "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"Animal\") [JSIdentifier \"name\"] (JSFunctionBody [JSSourceElements [JSIf (JSExpression [JSUnary \"!\",JSIdentifier \"name\"]) (JSBlock (JSStatementList [JSThrow (JSExpression [JSLiteral \"new \",JSIdentifier \"Error\",JSArguments [[JSStringLiteral '\\'' \"Must specify an animal name\"]]])])),JSExpression [JSMemberDot [JSLiteral \"this\"] (JSIdentifier \"name\"),JSOperator \"=\",JSIdentifier \"name\"]]]),JSLiteral \";\",JSExpression [JSMemberDot [JSMemberDot [JSIdentifier \"Animal\"] (JSIdentifier \"prototype\")] (JSIdentifier \"toString\"),JSOperator \"=\",JSFunctionExpression [] [] (JSFunctionBody [JSSourceElements [JSReturn [JSExpression [JSMemberDot [JSLiteral \"this\"] (JSIdentifier \"name\")],JSLiteral \"\"]]])],JSLiteral \";\",JSExpression [JSIdentifier \"o\",JSOperator \"=\",JSLiteral \"new \",JSIdentifier \"Animal\",JSArguments [[JSStringLiteral '\"' \"bob\"]]],JSLiteral \";\",JSExpression [JSExpressionBinary \"==\" [JSMemberDot [JSIdentifier \"o\"] (JSIdentifier \"toString\"),JSArguments []] [JSStringLiteral '\"' \"bob\"]]])")

    , testCase "min_100_animals2" (testProg "Animal=function(){return this.name};" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"Animal\",JSOperator \"=\",JSFunctionExpression [] [] (JSFunctionBody [JSSourceElements [JSReturn [JSExpression [JSMemberDot [JSLiteral \"this\"] (JSIdentifier \"name\")],JSLiteral \"\"]]])],JSLiteral \";\"])")

    , testCase "min_100_animals3" (testProg "if(a)x=1;y=2" "Right (JSSourceElementsTop [JSIf (JSExpression [JSIdentifier \"a\"]) (JSBlock (JSStatementList [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"]])),JSExpression [JSIdentifier \"y\",JSOperator \"=\",JSDecimal \"2\"]])")

    , testCase "min_100_animals4" (testProg "if(a)x=a()y=2" "Right (JSSourceElementsTop [JSIf (JSExpression [JSIdentifier \"a\"]) (JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSIdentifier \"a\",JSArguments []]),JSExpression [JSIdentifier \"y\",JSOperator \"=\",JSDecimal \"2\"]])")

    , testCase "05_regex"  (testProg "newlines=spaces.match(/\\n/g)" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"newlines\",JSOperator \"=\",JSMemberDot [JSIdentifier \"spaces\"] (JSIdentifier \"match\"),JSArguments [[JSRegEx \"/\\\\n/g\"]]]])")

    , testCase "05_regex2" (testProg "x=/\\n/g" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSRegEx \"/\\\\n/g\"]])")

    , testCase "05_regex3" (testProg "x=i(/[?|^&(){}\\[\\]+\\-*\\/\\.]/g,\"\\\\$&\")" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSIdentifier \"i\",JSArguments [[JSRegEx \"/[?|^&(){}\\\\[\\\\]+\\\\-*\\\\/\\\\.]/g\"],[JSStringLiteral '\"' \"\\\\\\\\$&\"]]]])")

    , testCase "05_regex4" (testProg "x=i(/^$/g,\"\\\\$&\")" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSIdentifier \"i\",JSArguments [[JSRegEx \"/^$/g\"],[JSStringLiteral '\"' \"\\\\\\\\$&\"]]]])")

   , testCase "05_regex5" (testProg "if(/^[a-z]/.test(t)){consts+=t.toUpperCase();keywords[t]=i}else consts+=(/^\\W/.test(t)?opTypeNames[t]:t);" "Right (JSSourceElementsTop [JSIfElse (JSExpression [JSMemberDot [JSRegEx \"/^[a-z]/\"] (JSIdentifier \"test\"),JSArguments [[JSIdentifier \"t\"]]]) (JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"consts\",JSOperator \"+=\",JSMemberDot [JSIdentifier \"t\"] (JSIdentifier \"toUpperCase\"),JSArguments []],JSLiteral \";\",JSExpression [JSMemberSquare [JSIdentifier \"keywords\"] (JSExpression [JSIdentifier \"t\"]),JSOperator \"=\",JSIdentifier \"i\"]])) (JSExpression [JSIdentifier \"consts\",JSOperator \"+=\",JSExpressionParen (JSExpression [JSExpressionTernary [JSMemberDot [JSRegEx \"/^\\\\W/\"] (JSIdentifier \"test\"),JSArguments [[JSIdentifier \"t\"]]] [JSMemberSquare [JSIdentifier \"opTypeNames\"] (JSExpression [JSIdentifier \"t\"])] [JSIdentifier \"t\"]])]),JSLiteral \";\"])")

   , testCase "if_semi" (testProg "if(x);x=1" "Right (JSSourceElementsTop [JSIf (JSExpression [JSIdentifier \"x\"]) (JSLiteral \";\"),JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"]])")

   , testCase "67_bob" (testProg "(match = /^\"(?:\\\\.|[^\"])*\"|^'(?:[^']|\\\\.)*'/(input))" "Right (JSSourceElementsTop [JSExpression [JSExpressionParen (JSExpression [JSIdentifier \"match\",JSOperator \"=\",JSRegEx \"/^\\\"(?:\\\\\\\\.|[^\\\"])*\\\"|^'(?:[^']|\\\\\\\\.)*'/\",JSArguments [[JSIdentifier \"input\"]]])]])")

   , testCase "122_jsexec" (testProg "v = getValue(execute(n[0], x)) in getValue(execute(n[1], x));" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"v\",JSOperator \"=\",JSExpressionBinary \" in \" [JSIdentifier \"getValue\",JSArguments [[JSIdentifier \"execute\",JSArguments [[JSMemberSquare [JSIdentifier \"n\"] (JSExpression [JSDecimal \"0\"])],[JSIdentifier \"x\"]]]]] [JSIdentifier \"getValue\",JSArguments [[JSIdentifier \"execute\",JSArguments [[JSMemberSquare [JSIdentifier \"n\"] (JSExpression [JSDecimal \"1\"])],[JSIdentifier \"x\"]]]]]],JSLiteral \";\"])")

   , testCase "bug1" (testProg "/* */\nfunction f() {\n/*  */\n}\n" "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"f\") [] (JSFunctionBody [])])")
   , testCase "bug1" (testProg "/* **/\nfunction f() {\n/*  */\n}\n" "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"f\") [] (JSFunctionBody [])])")

   , testCase "unicode1-ws" (testProg "a \f\v\t\r\n=\x00a0\x1680\x180e\x2000\x2001\x2002\x2003\x2004\x2005\x2006\x2007\x2008\x2009\x200a\x2028\x2029\x202f\x205f\x3000\&1;" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"a\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\"])")

   , testCase "unicode2-lt" (testProg "//comment\x000Ax=1;" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\"])")
   , testCase "unicode3-lt" (testProg "//comment\x000Dx=1;" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\"])")
   , testCase "unicode4-lt" (testProg "//comment\x2028x=1;" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\"])")
   , testCase "unicode5-lt" (testProg "//comment\x2029x=1;" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\"])")

   , testCase "unicode2" (testProg "àáâãäå = 1;" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"\\224\\225\\226\\227\\228\\229\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\"])")

   , testCase "unicode3" (testProg "$aà = 1;_b=2;\0065a=2"  "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"$a\\224\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\",JSExpression [JSIdentifier \"_b\",JSOperator \"=\",JSDecimal \"2\"],JSLiteral \";\",JSExpression [JSIdentifier \"Aa\",JSOperator \"=\",JSDecimal \"2\"]])")

   , testCase "unicode4" (testProg "x=\"àáâãäå\";y='\3012a\0068'" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSStringLiteral '\"' \"\\224\\225\\226\\227\\228\\229\"],JSLiteral \";\",JSExpression [JSIdentifier \"y\",JSOperator \"=\",JSStringLiteral '\\'' \"\\3012aD\"]])")

   , testCase "unicode5" (testFile "./test/Unicode.js" "JSSourceElementsTop [JSExpression [JSIdentifier \"\\224\\225\\226\\227\\228\\229\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\"]")

   , testCase "bug2.a" (testProg "function() {\nz = function /*z*/(o) {\nreturn r;\n};}" "Right (JSSourceElementsTop [JSExpression [JSFunctionExpression [] [] (JSFunctionBody [JSSourceElements [JSExpression [JSIdentifier \"z\",JSOperator \"=\",JSFunctionExpression [] [JSIdentifier \"o\"] (JSFunctionBody [JSSourceElements [JSReturn [JSExpression [JSIdentifier \"r\"],JSLiteral \";\"]]])],JSLiteral \";\"]])]])")

   , testCase "bug2.b" (testProg "function() {\nz = function z(o) {\nreturn r;\n};}" "Right (JSSourceElementsTop [JSExpression [JSFunctionExpression [] [] (JSFunctionBody [JSSourceElements [JSExpression [JSIdentifier \"z\",JSOperator \"=\",JSFunctionExpression [JSIdentifier \"z\"] [JSIdentifier \"o\"] (JSFunctionBody [JSSourceElements [JSReturn [JSExpression [JSIdentifier \"r\"],JSLiteral \";\"]]])],JSLiteral \";\"]])]])")

   -- https://github.com/alanz/hjsmin/issues/#issue/3
   , testCase "bug3" (testProg "var myLatlng = new google.maps.LatLng(56.8379100, 60.5806664);" "Right (JSSourceElementsTop [JSVariables \"var\" [JSVarDecl (JSIdentifier \"myLatlng\") [JSLiteral \"new \",JSMemberDot [JSMemberDot [JSIdentifier \"google\"] (JSIdentifier \"maps\")] (JSIdentifier \"LatLng\"),JSArguments [[JSDecimal \"56.8379100\"],[JSDecimal \"60.5806664\"]]]]])")

   -- https://github.com/alanz/hjsmin/issues/#issue/4
   , testCase "bug4" (testProg "/* * geolocation. пытаемся определить свое местоположение * если не получается то используем defaultLocation * @Param {object} map экземпляр карты * @Param {object LatLng} defaultLocation Координаты центра по умолчанию * @Param {function} callbackAfterLocation Фу-ия которая вызывается после * геолокации. Т.к запрос геолокации асинхронен */x" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\"]])")



   , testCase "02_sm.js"   (testProg "{zero}\none1;two\n{three\nfour;five;\n{\nsix;{seven;}\n}\n}" "Right (JSSourceElementsTop [JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"zero\"]]),JSExpression [JSIdentifier \"one1\"],JSLiteral \";\",JSExpression [JSIdentifier \"two\"],JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"three\"],JSExpression [JSIdentifier \"four\"],JSLiteral \";\",JSExpression [JSIdentifier \"five\"],JSLiteral \";\",JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"six\"],JSLiteral \";\",JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"seven\"],JSLiteral \";\"])])])])")

   , testCase "02_sm.js.2" (testProg "{zero}\nget;two\n{three\nfour;set;\n{\nsix;{seven;}\n}\n}" "Right (JSSourceElementsTop [JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"zero\"]]),JSExpression [JSIdentifier \"get\"],JSLiteral \";\",JSExpression [JSIdentifier \"two\"],JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"three\"],JSExpression [JSIdentifier \"four\"],JSLiteral \";\",JSExpression [JSIdentifier \"set\"],JSLiteral \";\",JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"six\"],JSLiteral \";\",JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"seven\"],JSLiteral \";\"])])])])")

   , testCase "loc1" (testProgUn "x = 1\n  y=2;" "Right (NS (JSSourceElementsTop [NS (JSExpression [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [NoComment],NS (JSOperator \"=\") (TokenPn 2 1 3) [NoComment],NS (JSDecimal \"1\") (TokenPn 4 1 5) [NoComment]]) (TokenPn 0 1 1) [],NS (JSExpression [NS (JSIdentifier \"y\") (TokenPn 8 2 3) [NoComment],NS (JSOperator \"=\") (TokenPn 9 2 4) [NoComment],NS (JSDecimal \"2\") (TokenPn 10 2 5) [NoComment]]) (TokenPn 8 2 3) [],NS (JSLiteral \";\") (TokenPn 11 2 6) [NoComment]]) (TokenPn 0 1 1) [])")

   -- https://github.com/alanz/language-javascript/issues/2
   , testCase "issue2" (testProg "var img = document.createElement('img');\nimg.src = \"mylogo.jpg\";\n$(img).click(function() {\n   alert('clicked!');\n});" "Right (JSSourceElementsTop [JSVariables \"var\" [JSVarDecl (JSIdentifier \"img\") [JSMemberDot [JSIdentifier \"document\"] (JSIdentifier \"createElement\"),JSArguments [[JSStringLiteral '\\'' \"img\"]]]],JSExpression [JSMemberDot [JSIdentifier \"img\"] (JSIdentifier \"src\"),JSOperator \"=\",JSStringLiteral '\"' \"mylogo.jpg\"],JSLiteral \";\",JSExpression [JSIdentifier \"$\",JSArguments [[JSIdentifier \"img\"]],JSCallExpression \".\" [JSIdentifier \"click\"],JSCallExpression \"()\" [JSArguments [[JSFunctionExpression [] [] (JSFunctionBody [JSSourceElements [JSExpression [JSIdentifier \"alert\",JSArguments [[JSStringLiteral '\\'' \"clicked!\"]]],JSLiteral \";\"]])]]]],JSLiteral \";\"])")


   -- Working in ECMASCRIPT 5.1 changes
   , testCase "lineTerminatorInString1" (testProg "x='abc\\\ndef';"    "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSStringLiteral '\\'' \"abcdef\"],JSLiteral \";\"])")
   , testCase "lineTerminatorInString2" (testProg "x=\"abc\\\ndef\";" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSStringLiteral '\"' \"abcdef\"],JSLiteral \";\"])")
   , testCase "lineTerminatorInString3" (testProg "x=\"abc\\\rdef\";" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSStringLiteral '\"' \"abcdef\"],JSLiteral \";\"])")
   , testCase "lineTerminatorInString4" (testProg "x=\"abc\\\x2028 def\";" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSStringLiteral '\"' \"abc def\"],JSLiteral \";\"])")
   , testCase "lineTerminatorInString5" (testProg "x=\"abc\\\x2029 def\";" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSStringLiteral '\"' \"abc def\"],JSLiteral \";\"])")
   , testCase "lineTerminatorInString6" (testProg "x=\"abc\\\r\ndef\";" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSStringLiteral '\"' \"abcdef\"],JSLiteral \";\"])")


     -- https://github.com/alanz/language-javascript/issues/4
   , testCase "issue4ok"   (testProg "var k = {\ny: somename\n}" "Right (JSSourceElementsTop [JSVariables \"var\" [JSVarDecl (JSIdentifier \"k\") [JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"y\") [JSIdentifier \"somename\"]]]]])")
   , testCase "issue4bug1" (testProg "var k = {\ny: code\n}" "Right (JSSourceElementsTop [JSVariables \"var\" [JSVarDecl (JSIdentifier \"k\") [JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"y\") [JSIdentifier \"code\"]]]]])")
   , testCase "issue4bug2" (testProg "var k = {\ny: mode\n}" "Right (JSSourceElementsTop [JSVariables \"var\" [JSVarDecl (JSIdentifier \"k\") [JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"y\") [JSIdentifier \"mode\"]]]]])")

     -- https://github.com/alanz/language-javascript/issues/5
   , testCase "issue5bug1" (testProg "x = { y: 1e8 }" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"y\") [JSDecimal \"1e8\"]]]])")
   , testCase "issue5ok2" (testProg "{ y: 1e8 }" "Right (JSSourceElementsTop [JSStatementBlock (JSStatementList [JSLabelled (JSIdentifier \"y\") (JSExpression [JSDecimal \"1e8\"])])])")
   , testCase "issue5ok3" (testProg "{ y: 18 }" "Right (JSSourceElementsTop [JSStatementBlock (JSStatementList [JSLabelled (JSIdentifier \"y\") (JSExpression [JSDecimal \"18\"])])])")
   , testCase "issue5ok4" (testProg "x = { y: 18 }" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator \"=\",JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"y\") [JSDecimal \"18\"]]]])")
 -}
    ]

-- ---------------------------------------------------------------------
-- Test utilities

testRoundTrip str = str @=? (renderToString $ readJs str)

testLiteral  literal expected = expected @=? (showStrippedMaybe $ parseUsing parseLiteral literal "src")
testLiteralC literal expected = expected @=? (show              $ parseUsing parseLiteral literal "src")


testPE  str expected = expected @=? (showStrippedMaybe $ parseUsing parsePrimaryExpression str "src")
testPEC str expected = expected @=? (show              $ parseUsing parsePrimaryExpression str "src")

testStmt  str expected = expected @=? (showStrippedMaybe $ parseUsing parseStatement str "src")
testStmtC str expected = expected @=? (show              $ parseUsing parseStatement str "src")

--testProg str expected = expected @=? (show $ parseUsing parseProgram str "src")
testProg  str expected = expected @=? (showStrippedMaybe $ parseUsing parseProgram str "src")
testProgC str expected = expected @=? (show              $ parseUsing parseProgram str "src")

testProgUn str expected = expected @=? (show $ parseUsing parseProgram str "src")


testFile fileName expected = do
  res <- parseFile fileName
  -- expected @=? (liftM show $ parseFile fileName)
  (expected @=? (showStripped res))


-- Set emacs mode
-- Local Variables:
-- coding: utf-8
-- End:

-- EOF
