
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)


import Control.Monad (liftM)
import Language.JavaScript.Parser.Parser
--import Language.JavaScript.Parser.Grammar
import Language.JavaScript.Parser.Grammar5

main :: IO ()
main = defaultMain [testSuite]

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
      
    , testCase "Switch1" (testStmt "switch (x) {}" "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) [])")
    , testCase "Switch2" (testStmt "switch (x) {case 1:break;}"          "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) [JSCase (JSExpression [JSDecimal \"1\"]) (JSStatementList [JSBreak [] [JSLiteral \";\"]])])")
    , testCase "Switch3" (testStmt "switch (x) {case 0:\ncase 1:break;}"  "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) [JSCase (JSExpression [JSDecimal \"0\"]) (JSStatementList []),JSCase (JSExpression [JSDecimal \"1\"]) (JSStatementList [JSBreak [] [JSLiteral \";\"]])])")                          
    , testCase "Switch4" (testStmt "switch (x) {default:break;}"          "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) [JSDefault (JSStatementList [JSBreak [] [JSLiteral \";\"]])])")
    , testCase "Switch5" (testStmt "switch (x) {default:\ncase 1:break;}"   "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) [JSDefault (JSStatementList []),JSCase (JSExpression [JSDecimal \"1\"]) (JSStatementList [JSBreak [] [JSLiteral \";\"]])])")
      
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
     
   , testCase "02_sm.js"   (testProg "{zero}\none1;two\n{three\nfour;five;\n{\nsix;{seven;}\n}\n}" "Right (JSSourceElementsTop [JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"zero\"]]),JSExpression [JSIdentifier \"one1\"],JSLiteral \";\",JSExpression [JSIdentifier \"two\"],JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"three\"],JSExpression [JSIdentifier \"four\"],JSLiteral \";\",JSExpression [JSIdentifier \"five\"],JSLiteral \";\",JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"six\"],JSLiteral \";\",JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"seven\"],JSLiteral \";\"])])])])")
     
   , testCase "02_sm.js.2" (testProg "{zero}\nget;two\n{three\nfour;set;\n{\nsix;{seven;}\n}\n}" "Right (JSSourceElementsTop [JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"zero\"]]),JSExpression [JSIdentifier \"get\"],JSLiteral \";\",JSExpression [JSIdentifier \"two\"],JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"three\"],JSExpression [JSIdentifier \"four\"],JSLiteral \";\",JSExpression [JSIdentifier \"set\"],JSLiteral \";\",JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"six\"],JSLiteral \";\",JSStatementBlock (JSStatementList [JSExpression [JSIdentifier \"seven\"],JSLiteral \";\"])])])])")

   , testCase "loc1" (testProgUn "x = 1\n  y=2;" "Right (NS (JSSourceElementsTop [NS (JSExpression [NS (JSIdentifier \"x\") (SpanPoint {span_filename = \"\", span_row = 1, span_column = 1}),NS (JSOperator \"=\") (SpanPoint {span_filename = \"\", span_row = 1, span_column = 3}),NS (JSDecimal \"1\") (SpanPoint {span_filename = \"\", span_row = 1, span_column = 5})]) (SpanPoint {span_filename = \"\", span_row = 1, span_column = 1}),NS (JSExpression [NS (JSIdentifier \"y\") (SpanPoint {span_filename = \"\", span_row = 2, span_column = 3}),NS (JSOperator \"=\") (SpanPoint {span_filename = \"\", span_row = 2, span_column = 4}),NS (JSDecimal \"2\") (SpanPoint {span_filename = \"\", span_row = 2, span_column = 5})]) (SpanPoint {span_filename = \"\", span_row = 2, span_column = 3}),NS (JSLiteral \";\") (SpanPoint {span_filename = \"\", span_row = 2, span_column = 6})]) (SpanPoint {span_filename = \"\", span_row = 1, span_column = 1}))")
     
    ]

srcHelloWorld = "Hello"
caseHelloWorld =  
  "JSSourceElementsTop [JSExpression [JSIdentifier \"Hello\"]]"
  -- @=? (show $ parse srcHelloWorld "src")
  @=? (showStripped $ readJs srcHelloWorld)
  

-- ---------------------------------------------------------------------
-- Test utilities

testLiteral literal expected = expected @=? (showStrippedMaybe $ parseUsing parseLiteral literal "src")

testPE str expected = expected @=? (showStrippedMaybe $ parseUsing parsePrimaryExpression str "src")

testStmt str expected = expected @=? (showStrippedMaybe $ parseUsing parseStatement str "src")

--testProg str expected = expected @=? (show $ parseUsing parseProgram str "src")
testProg str expected = expected @=? (showStrippedMaybe $ parseUsing parseProgram str "src")

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
