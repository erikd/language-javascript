module Test.Language.Javascript.ExpressionParser
    ( testExpressionParser
    ) where

import Test.Hspec

import Language.JavaScript.Parser
import Language.JavaScript.Parser.Grammar7
import Language.JavaScript.Parser.Parser


testExpressionParser :: Spec
testExpressionParser = describe "Parse expressions:" $ do
    it "this" $
        testExpr "this"     `shouldBe` "Right (JSAstExpression (JSLiteral 'this'))"
    it "regex" $ do
        testExpr "/blah/"   `shouldBe` "Right (JSAstExpression (JSRegEx '/blah/'))"
        testExpr "/$/g"     `shouldBe` "Right (JSAstExpression (JSRegEx '/$/g'))"
        testExpr "/\\n/g"   `shouldBe` "Right (JSAstExpression (JSRegEx '/\\n/g'))"
        testExpr "/(\\/)/"  `shouldBe` "Right (JSAstExpression (JSRegEx '/(\\/)/'))"
        testExpr "/a[/]b/"  `shouldBe` "Right (JSAstExpression (JSRegEx '/a[/]b/'))"
        testExpr "/[/\\]/"  `shouldBe` "Right (JSAstExpression (JSRegEx '/[/\\]/'))"
        testExpr "/(\\/|\\)/"  `shouldBe` "Right (JSAstExpression (JSRegEx '/(\\/|\\)/'))"
        testExpr "/a\\[|\\]$/g" `shouldBe` "Right (JSAstExpression (JSRegEx '/a\\[|\\]$/g'))"
        testExpr "/[(){}\\[\\]]/g" `shouldBe` "Right (JSAstExpression (JSRegEx '/[(){}\\[\\]]/g'))"
        testExpr "/^\"(?:\\.|[^\"])*\"|^'(?:[^']|\\.)*'/" `shouldBe` "Right (JSAstExpression (JSRegEx '/^\"(?:\\.|[^\"])*\"|^'(?:[^']|\\.)*'/'))"

    it "identifier" $ do
        testExpr "_$"       `shouldBe` "Right (JSAstExpression (JSIdentifier '_$'))"
        testExpr "this_"    `shouldBe` "Right (JSAstExpression (JSIdentifier 'this_'))"
    it "array literal" $ do
        testExpr "[]"       `shouldBe` "Right (JSAstExpression (JSArrayLiteral []))"
        testExpr "[,]"      `shouldBe` "Right (JSAstExpression (JSArrayLiteral [JSComma]))"
        testExpr "[,,]"     `shouldBe` "Right (JSAstExpression (JSArrayLiteral [JSComma,JSComma]))"
        testExpr "[,,x]"    `shouldBe` "Right (JSAstExpression (JSArrayLiteral [JSComma,JSComma,JSIdentifier 'x']))"
        testExpr "[,,x]"    `shouldBe` "Right (JSAstExpression (JSArrayLiteral [JSComma,JSComma,JSIdentifier 'x']))"
        testExpr "[,x,,x]"  `shouldBe` "Right (JSAstExpression (JSArrayLiteral [JSComma,JSIdentifier 'x',JSComma,JSComma,JSIdentifier 'x']))"
        testExpr "[x]"      `shouldBe` "Right (JSAstExpression (JSArrayLiteral [JSIdentifier 'x']))"
        testExpr "[x,]"     `shouldBe` "Right (JSAstExpression (JSArrayLiteral [JSIdentifier 'x',JSComma]))"
        testExpr "[,,,]"    `shouldBe` "Right (JSAstExpression (JSArrayLiteral [JSComma,JSComma,JSComma]))"
        testExpr "[a,,]"    `shouldBe` "Right (JSAstExpression (JSArrayLiteral [JSIdentifier 'a',JSComma,JSComma]))"
    it "operator precedence" $
        testExpr "2+3*4+5"  `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('+',JSExpressionBinary ('+',JSDecimal '2',JSExpressionBinary ('*',JSDecimal '3',JSDecimal '4')),JSDecimal '5')))"
    it "parentheses" $
        testExpr "(56)"     `shouldBe` "Right (JSAstExpression (JSExpressionParen (JSDecimal '56')))"
    it "string concatenation" $ do
        testExpr "'ab' + 'bc'"  `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('+',JSStringLiteral 'ab',JSStringLiteral 'bc')))"
        testExpr "'bc' + \"cd\""  `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('+',JSStringLiteral 'bc',JSStringLiteral \"cd\")))"
    it "object literal" $ do
        testExpr "{}"           `shouldBe` "Right (JSAstExpression (JSObjectLiteral []))"
        testExpr "{x:1}"        `shouldBe` "Right (JSAstExpression (JSObjectLiteral [JSPropertyNameandValue (JSIdentifier 'x') [JSDecimal '1']]))"
        testExpr "{x:1,y:2}"    `shouldBe` "Right (JSAstExpression (JSObjectLiteral [JSPropertyNameandValue (JSIdentifier 'x') [JSDecimal '1'],JSPropertyNameandValue (JSIdentifier 'y') [JSDecimal '2']]))"
        testExpr "{x:1,}"       `shouldBe` "Right (JSAstExpression (JSObjectLiteral [JSPropertyNameandValue (JSIdentifier 'x') [JSDecimal '1'],JSComma]))"
        testExpr "a={if:1,interface:2}" `shouldBe` "Right (JSAstExpression (JSOpAssign ('=',JSIdentifier 'a',JSObjectLiteral [JSPropertyNameandValue (JSIdentifier 'if') [JSDecimal '1'],JSPropertyNameandValue (JSIdentifier 'interface') [JSDecimal '2']])))"
        testExpr "a={\n  values: 7,\n}\n"   `shouldBe` "Right (JSAstExpression (JSOpAssign ('=',JSIdentifier 'a',JSObjectLiteral [JSPropertyNameandValue (JSIdentifier 'values') [JSDecimal '7'],JSComma])))"
        testExpr "x={get foo() {return 1},set foo(a) {x=a}}" `shouldBe` "Right (JSAstExpression (JSOpAssign ('=',JSIdentifier 'x',JSObjectLiteral [JSPropertyAccessor JSAccessorGet (JSIdentifier 'foo') [] (JSBlock [JSReturn JSDecimal '1' ]),JSPropertyAccessor JSAccessorSet (JSIdentifier 'foo') [JSIdentifier 'a'] (JSBlock [JSOpAssign ('=',JSIdentifier 'x',JSIdentifier 'a')])])))"
        testExpr "{evaluate:evaluate,load:function load(s){if(x)return s;1}}" `shouldBe` "Right (JSAstExpression (JSObjectLiteral [JSPropertyNameandValue (JSIdentifier 'evaluate') [JSIdentifier 'evaluate'],JSPropertyNameandValue (JSIdentifier 'load') [JSFunctionExpression 'load' (JSIdentifier 's') (JSBlock [JSIf (JSIdentifier 'x') (JSReturn JSIdentifier 's' JSSemicolon),JSDecimal '1']))]]))"
        testExpr "obj = { name : 'A', 'str' : 'B', 123 : 'C', }" `shouldBe` "Right (JSAstExpression (JSOpAssign ('=',JSIdentifier 'obj',JSObjectLiteral [JSPropertyNameandValue (JSIdentifier 'name') [JSStringLiteral 'A'],JSPropertyNameandValue (JSIdentifier ''str'') [JSStringLiteral 'B'],JSPropertyNameandValue (JSIdentifier '123') [JSStringLiteral 'C'],JSComma])))"

    it "unary expression" $ do
        testExpr "delete y" `shouldBe` "Right (JSAstExpression (JSUnaryExpression ('delete',JSIdentifier 'y')))"
        testExpr "void y"   `shouldBe` "Right (JSAstExpression (JSUnaryExpression ('void',JSIdentifier 'y')))"
        testExpr "typeof y" `shouldBe` "Right (JSAstExpression (JSUnaryExpression ('typeof',JSIdentifier 'y')))"
        testExpr "++y"      `shouldBe` "Right (JSAstExpression (JSUnaryExpression ('++',JSIdentifier 'y')))"
        testExpr "--y"      `shouldBe` "Right (JSAstExpression (JSUnaryExpression ('--',JSIdentifier 'y')))"
        testExpr "+y"       `shouldBe` "Right (JSAstExpression (JSUnaryExpression ('+',JSIdentifier 'y')))"
        testExpr "-y"       `shouldBe` "Right (JSAstExpression (JSUnaryExpression ('-',JSIdentifier 'y')))"
        testExpr "~y"       `shouldBe` "Right (JSAstExpression (JSUnaryExpression ('~',JSIdentifier 'y')))"
        testExpr "!y"       `shouldBe` "Right (JSAstExpression (JSUnaryExpression ('!',JSIdentifier 'y')))"
        testExpr "y++"      `shouldBe` "Right (JSAstExpression (JSExpressionPostfix ('++',JSIdentifier 'y')))"
        testExpr "y--"      `shouldBe` "Right (JSAstExpression (JSExpressionPostfix ('--',JSIdentifier 'y')))"
        testExpr "...y"     `shouldBe` "Right (JSAstExpression (JSSpreadExpression (JSIdentifier 'y')))"


    it "new expression" $ do
        testExpr "new x()"  `shouldBe` "Right (JSAstExpression (JSMemberNew (JSIdentifier 'x',JSArguments ())))"
        testExpr "new x.y"  `shouldBe` "Right (JSAstExpression (JSNewExpression JSMemberDot (JSIdentifier 'x',JSIdentifier 'y')))"

    it "binary expression" $ do
        testExpr "x||y"     `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('||',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x&&y"     `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('&&',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x|y"      `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('|',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x^y"      `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('^',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x&y"      `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('&',JSIdentifier 'x',JSIdentifier 'y')))"

        testExpr "x==y"     `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('==',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x!=y"     `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('!=',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x===y"    `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('===',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x!==y"    `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('!==',JSIdentifier 'x',JSIdentifier 'y')))"

        testExpr "x<y"      `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('<',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x>y"      `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('>',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x<=y"     `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('<=',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x>=y"     `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('>=',JSIdentifier 'x',JSIdentifier 'y')))"

        testExpr "x<<y"     `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('<<',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x>>y"     `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('>>',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x>>>y"    `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('>>>',JSIdentifier 'x',JSIdentifier 'y')))"

        testExpr "x+y"      `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('+',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x-y"      `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('-',JSIdentifier 'x',JSIdentifier 'y')))"

        testExpr "x*y"      `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('*',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x/y"      `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('/',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x%y"      `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('%',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x instanceof y" `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('instanceof',JSIdentifier 'x',JSIdentifier 'y')))"

    it "assign expression" $ do
        testExpr "x=1"          `shouldBe` "Right (JSAstExpression (JSOpAssign ('=',JSIdentifier 'x',JSDecimal '1')))"
        testExpr "x*=1"         `shouldBe` "Right (JSAstExpression (JSOpAssign ('*=',JSIdentifier 'x',JSDecimal '1')))"
        testExpr "x/=1"         `shouldBe` "Right (JSAstExpression (JSOpAssign ('/=',JSIdentifier 'x',JSDecimal '1')))"
        testExpr "x%=1"         `shouldBe` "Right (JSAstExpression (JSOpAssign ('%=',JSIdentifier 'x',JSDecimal '1')))"
        testExpr "x+=1"         `shouldBe` "Right (JSAstExpression (JSOpAssign ('+=',JSIdentifier 'x',JSDecimal '1')))"
        testExpr "x-=1"         `shouldBe` "Right (JSAstExpression (JSOpAssign ('-=',JSIdentifier 'x',JSDecimal '1')))"
        testExpr "x<<=1"        `shouldBe` "Right (JSAstExpression (JSOpAssign ('<<=',JSIdentifier 'x',JSDecimal '1')))"
        testExpr "x>>=1"        `shouldBe` "Right (JSAstExpression (JSOpAssign ('>>=',JSIdentifier 'x',JSDecimal '1')))"
        testExpr "x>>>=1"       `shouldBe` "Right (JSAstExpression (JSOpAssign ('>>>=',JSIdentifier 'x',JSDecimal '1')))"
        testExpr "x&=1"         `shouldBe` "Right (JSAstExpression (JSOpAssign ('&=',JSIdentifier 'x',JSDecimal '1')))"
        testExpr "x^=1"         `shouldBe` "Right (JSAstExpression (JSOpAssign ('^=',JSIdentifier 'x',JSDecimal '1')))"
        testExpr "x|=1"         `shouldBe` "Right (JSAstExpression (JSOpAssign ('|=',JSIdentifier 'x',JSDecimal '1')))"

    it "function expression" $ do
        testExpr "function(){}"     `shouldBe` "Right (JSAstExpression (JSFunctionExpression '' () (JSBlock []))))"
        testExpr "function(a){}"    `shouldBe` "Right (JSAstExpression (JSFunctionExpression '' (JSIdentifier 'a') (JSBlock []))))"
        testExpr "function(a,b){}"  `shouldBe` "Right (JSAstExpression (JSFunctionExpression '' (JSIdentifier 'a',JSIdentifier 'b') (JSBlock []))))"
        testExpr "(a,b) => {}"  `shouldBe` "Right (JSAstExpression (JSArrowExpression ((JSIdentifier 'a',JSIdentifier 'b')) => JSBlock []))"
        testExpr "(a,b) => a + b"  `shouldBe` "Right (JSAstExpression (JSArrowExpression ((JSIdentifier 'a',JSIdentifier 'b')) => JSExpressionBinary ('+',JSIdentifier 'a',JSIdentifier 'b')))"

    it "member expression" $ do
        testExpr "x[y]"         `shouldBe` "Right (JSAstExpression (JSMemberSquare (JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x[y][z]"      `shouldBe` "Right (JSAstExpression (JSMemberSquare (JSMemberSquare (JSIdentifier 'x',JSIdentifier 'y'),JSIdentifier 'z')))"
        testExpr "x.y"          `shouldBe` "Right (JSAstExpression (JSMemberDot (JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x.y.z"        `shouldBe` "Right (JSAstExpression (JSMemberDot (JSMemberDot (JSIdentifier 'x',JSIdentifier 'y'),JSIdentifier 'z')))"

    it "call expression" $ do
        testExpr "x()"         `shouldBe` "Right (JSAstExpression (JSMemberExpression (JSIdentifier 'x',JSArguments ())))"
        testExpr "x()()"       `shouldBe` "Right (JSAstExpression (JSCallExpression (JSMemberExpression (JSIdentifier 'x',JSArguments ()),JSArguments ())))"
        testExpr "x()[4]"      `shouldBe` "Right (JSAstExpression (JSCallExpressionSquare (JSMemberExpression (JSIdentifier 'x',JSArguments ()),JSDecimal '4')))"
        testExpr "x().x"       `shouldBe` "Right (JSAstExpression (JSCallExpressionDot (JSMemberExpression (JSIdentifier 'x',JSArguments ()),JSIdentifier 'x')))"
        testExpr "x(a,b=2).x"  `shouldBe` "Right (JSAstExpression (JSCallExpressionDot (JSMemberExpression (JSIdentifier 'x',JSArguments (JSIdentifier 'a',JSOpAssign ('=',JSIdentifier 'b',JSDecimal '2'))),JSIdentifier 'x')))"
        testExpr "foo (56.8379100, 60.5806664)" `shouldBe` "Right (JSAstExpression (JSMemberExpression (JSIdentifier 'foo',JSArguments (JSDecimal '56.8379100',JSDecimal '60.5806664'))))"

    it "spread expression" $
        testExpr "... x"        `shouldBe` "Right (JSAstExpression (JSSpreadExpression (JSIdentifier 'x')))"

testExpr :: String -> String
testExpr str = showStrippedMaybe (parseUsing parseExpression str "src")
