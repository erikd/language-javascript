{
module Language.JavaScript.Parser.Grammar (parse, parseLiteral, parsePrimaryExpression,parseStatement) where

import Control.Monad.Error.Class (throwError)
import Data.Char
import Language.JavaScript.Parser.Lexer
import Language.JavaScript.Parser.ParserMonad
import Language.JavaScript.Parser.SrcLocation
import qualified Language.JavaScript.Parser.AST as AST

}

-- The name of the generated function to be exported from the module
%name parse                  PrimaryExpression
%name parseLiteral           Literal
%name parsePrimaryExpression PrimaryExpression
%name parseStatement         Statement

%tokentype { Token }
%error { parseError }
%monad { P } { thenP } { returnP }
%lexer { lexCont } { EOFToken {} }


%token 

     ';'	{ SemiColonToken {} }
     ','	{ CommaToken {} }
     '?'	{ HookToken {} }
     ':'	{ ColonToken {} }
     '||'	{ OrToken {} }
     '&&'	{ AndToken {} }
     '|'	{ BitwiseOrToken {} }
     '^'	{ BitwiseXorToken {} }
     '&'	{ BitwiseAndToken {} }
     '==='	{ StrictEqToken {} }
     '=='	{ EqToken {} }
     '='	{ SimpleAssignToken {} }
     '!=='	{ StrictNeToken {} }
     '!='	{ NeToken {} }
     '<<'	{ LshToken {} }
     '<='	{ LeToken {} }
     '<'	{ LtToken {} }
     '>>>'	{ UrshToken {} }
     '>>'	{ RshToken {} }
     '>='	{ GeToken {} }
     '>'	{ GtToken {} }
     '++'	{ IncrementToken {} }
     '--'	{ DecrementToken {} }
     '+'	{ PlusToken {} }
     '-'	{ MinusToken {} }
     '*'	{ MulToken {} }
     '/'	{ DivToken {} }
     '%'	{ ModToken {} }
     '!'	{ NotToken {} }
     '~'	{ BitwiseNotToken {} }
     '.'	{ DotToken {} }
     '['	{ LeftBracketToken {} }
     ']'	{ RightBracketToken {} }
     '{'	{ LeftCurlyToken {} }
     '}'	{ RightCurlyToken {} }
     '('	{ LeftParenToken {} }
     ')'	{ RightParenToken {} }
     '@*/'	{ CondcommentEndToken {} }
  
     'break'      { BreakToken {} }
     'case'       { CaseToken {} }
     'catch'      { CatchToken {} }
     'const'      { ConstToken {} }
     'continue'   { ContinueToken {} }
     'debugger'   { DebuggerToken {} }
     'default'    { DefaultToken {} }
     'delete'     { DeleteToken {} }
     'do'         { DoToken {} }
     'else'       { ElseToken {} }
     'enum'       { EnumToken {} }
     'false'      { FalseToken {} }
     'finally'    { FinallyToken {} }
     'for'        { ForToken {} }
     'function'   { FunctionToken {} }
     'if'         { IfToken {} }
     'in'         { InToken {} }
     'instanceof' { InstanceofToken {} }
     'new'        { NewToken {} }
     'null'       { NullToken {} }
     'return'     { ReturnToken {} }
     'switch'     { SwitchToken {} }
     'this'       { ThisToken {} }
     'throw'      { ThrowToken {} }
     'true'       { TrueToken {} }
     'try'        { TryToken {} }
     'typeof'     { TypeofToken {} }
     'var'        { VarToken {} }
     'void'       { VoidToken {} }
     'while'      { WhileToken {} }
     'with'       { WithToken {} }
     
     
     'ident'      { IdentifierToken {} }
     'decimal'    { DecimalToken {} }
     'hexinteger' { HexIntegerToken {} }
     'string'     { StringToken {} }
     'regex'      { RegExToken {} }
     'assign'     { AssignToken {} }


%%


-- --------------------------------------------------------------------
-- Start of GOLD Grammar for Javascript, used as a base
-- --------------------------------------------------------------------

-- "Name"     = 'JavaScript Grammar'
-- "Author"   = 'M.Schnoor-Matriciani'
-- "Version"  = '0.9'
-- "About"    = 'JavaScript Grammar, Subset of ECMA Edition 3'

-- "Start Symbol" = <Program>
-- "Case Sensitive" = 'True'
 
-- ! ------------------------------------------------- Sets

-- {ID Head}      = {Letter} + [_] + [$]
-- {ID Tail}      = {Alphanumeric} + [_] + [$]
-- {String Chars1} = {Printable} + {HT} - ["\] 
-- {String Chars2} = {Printable} + {HT} - [\''] 
-- {Hex Digit}    = {Digit} + [ABCDEF] + [abcdef]
-- {RegExp Chars} = {Letter}+{Digit}+['^']+['$']+['*']+['+']+['?']+['{']+['}']+['|']+['-']+['.']+[',']+['#']+['[']+[']']+['_']+['<']+['>']
-- {Non Terminator} = {String Chars1} - {CR} - {LF}
-- {Non Zero Digits}={Digit}-[0]

-- ! ------------------------------------------------- Terminals

-- Identifier    = {ID Head}{ID Tail}*
-- StringLiteral = '"' ( {String Chars1} | '\' {Printable} )* '"' | '' ( {String Chars2} | '\' {Printable} )* ''

-- HexIntegerLiteral = '0x' {Hex Digit}+

-- RegExp         = '/' ({RegExp Chars} | '\' {Non Terminator})+ '/' ( 'g' | 'i' | 'm' )*
-- DecimalLiteral= {Non Zero Digits}+ '.' {Digit}* ('e' | 'E' ) {Non Zero Digits}+ {Digit}* |  {Non Zero Digits}+ '.' {Digit}* | '0' '.' {Digit}+ ('e' | 'E' ) {Non Zero Digits}+ {Digit}* | {Non Zero Digits}+ {Digit}* | '0' | '0' '.' {Digit}+

-- Comment Start = '/*'
-- Comment End   = '*/'
-- Comment Line  = '//'

-- ! ------------------------------------------------- Rules

-- <Literal> ::= <Null Literal>
--             | <Boolean Literal>
--             | <Numeric Literal>
--             | StringLiteral
Literal : NullLiteral     {$1}
        | BooleanLiteral  {$1}
        | NumericLiteral  {$1}
        | StringLiteral   {$1}


NullLiteral : 'null' { AST.JSLiteral "null" }

BooleanLiteral : 'true' { AST.JSLiteral "true" }
               | 'false' { AST.JSLiteral "false" }

-- <Numeric Literal> ::= DecimalLiteral
--                     | HexIntegerLiteral
NumericLiteral : 'decimal'    { AST.JSDecimal (token_literal $1)}
               | 'hexinteger' { AST.JSHexInteger (token_literal $1)}

StringLiteral : 'string'  {AST.JSStringLiteral (token_delimiter $1) (token_literal $1)}

-- <Regular Expression Literal> ::= RegExp 
RegularExpressionLiteral : 'regex' {AST.JSRegEx (token_literal $1)}

-- <Primary Expression> ::= 'this'
--                        | Identifier
--                        | <Literal> 
--                        | <Array Literal>
--                        | <Object Literal>
--                        | '(' <Expression> ')'
--                        | <Regular Expression Literal>
PrimaryExpression :: { AST.JSNode }
PrimaryExpression : 'this'                   { AST.JSLiteral "this" }
                  | Identifier               { $1 {- PrimaryExpression1 -}}
                  | Literal                  { $1 {- PrimaryExpression2 -}}
                  | ArrayLiteral             { $1 {- PrimaryExpression3 -}}
                  | ObjectLiteral            { $1 {- PrimaryExpression4 -}}
                  | '(' Expression ')'       { AST.JSExpressionParen $2 }
                  | RegularExpressionLiteral { $1 {- PrimaryExpression5 -}}
                  
Identifier : 'ident' { AST.JSIdentifier (token_literal $1) }

-- <Array Literal> ::= '[' ']'
--                   | '[' <Elision> ']'
--                   | '[' <Element List> ']'
--                   | '[' <Element List> ',' <Elision> ']'
ArrayLiteral : '[' ']'                         { AST.JSArrayLiteral [] }
             | '[' Elision ']'                 { AST.JSArrayLiteral $2 }
             | '[' ElementList ']'             { AST.JSArrayLiteral $2 }
             | '[' ElementList ',' Elision ']' { AST.JSArrayLiteral ($2++$4) }

-- <Elision> ::= ','
--             | <Elision> ','
Elision :  ','        { [(AST.JSElision [])] }
        | Elision ',' { $1 ++ [(AST.JSElision [])] }


-- <Element List> ::= <Elision> <Assignment Expression>
--                  | <Element List> ',' <Elision>  <Assignment Expression>
--                  | <Element List> ',' <Assignment Expression>
--                  | <Assignment Expression>
ElementList : Elision AssignmentExpression                    { [(AST.JSElementList ($1++$2)) ] }
            | ElementList ',' Elision  AssignmentExpression   { [(AST.JSElementList ($1++$3++$4))] }
            | ElementList ',' AssignmentExpression            { [(AST.JSElementList ($1++$3))] }
            | AssignmentExpression                            { $1 {- ElementList -}}

-- <Object Literal> ::= '{' <Property Name and Value List> '}'
ObjectLiteral :: { AST.JSNode }
ObjectLiteral : '{' PropertyNameandValueList '}' { AST.JSObjectLiteral $2 }

-- <Property Name and Value List> ::= <Property Name> ':' <Assignment Expression>
--                                  | <Property Name and Value List> ',' <Property Name> ':' <Assignment Expression>
PropertyNameandValueList :: { [ AST.JSNode ] }
PropertyNameandValueList : PropertyName ':' AssignmentExpression { [(AST.JSPropertyNameandValue $1 $3)] }
                         | PropertyNameandValueList ',' PropertyName ':' AssignmentExpression 
                           { ($1 ++ [(AST.JSPropertyNameandValue $3 $5)])  } 


-- <Property Name> ::= Identifier
--                   | StringLiteral
--                   | <Numeric Literal>
PropertyName : Identifier     { $1 {- PropertyName1 -}}
             | StringLiteral  { $1 {- PropertyName2 -}}
             | NumericLiteral { $1 {- PropertyName3 -}}

-- <Member Expression > ::= <Primary Expression>
--                        | <Function Expression>
--                        | <Member Expression> '[' <Expression> ']'
--                        | <Member Expression> '.' Identifier
--                        | 'new' <Member Expression> <Arguments>
MemberExpression :: { [AST.JSNode] }
MemberExpression : PrimaryExpression   { [$1] {- MemberExpression -}} -- TODO : uncomment rest, restore $1
                 | FunctionExpression  { [$1] {- MemberExpression -}}
                 | MemberExpression '[' Expression ']' { [AST.JSMemberSquare $3 $1] }
                 | MemberExpression '.' Identifier { [AST.JSMemberDot ($1++[$3])] } 
                 | 'new' MemberExpression Arguments { (((AST.JSLiteral "new "):$2)++[$3])}

-- <New Expression> ::= <Member Expression>
--                    | new <New Expression>
NewExpression : MemberExpression {$1 {- NewExpression -}} -- TODO: uncomment next
              | 'new' NewExpression { (AST.JSLiteral "new"):$2 }

-- <Call Expression> ::= <Member Expression> <Arguments>
--                     | <Call Expression> <Arguments> 
--                     | <Call Expression> '[' <Expression> ']'
--                     | <Call Expression> '.' Identifier
CallExpression :: { [AST.JSNode] }
CallExpression : MemberExpression Arguments        { $1++[$2] {- CallExpression -} } 
               | CallExpression Arguments          { ($1++[(AST.JSCallExpression "()" [$2])]) }
               | CallExpression '[' Expression ']' { ($1++[(AST.JSCallExpression "[]" [$3])]) }
               | CallExpression '.' Identifier     { ($1++[(AST.JSCallExpression "."  [$3])]) }


-- <Arguments> ::= '(' ')'
--               | '(' <Argument List> ')'
Arguments : '(' ')'               { (AST.JSArguments []) } 
          | '(' ArgumentList ')'  { (AST.JSArguments $2) }

-- <Argument List> ::= <Assignment Expression>
--                   | <Argument List> ',' <Assignment Expression>
ArgumentList :: { [[AST.JSNode]] }
ArgumentList : AssignmentExpression { [$1] {- ArgumentList -}}
             | ArgumentList ',' AssignmentExpression { $1++[$3] {- ArgumentList2 -} }


-- <Left Hand Side Expression> ::= <New Expression> 
--                               | <Call Expression>
LeftHandSideExpression : NewExpression  { $1 {- LeftHandSideExpression1 -}}
                       | CallExpression { $1 {- LeftHandSideExpression12 -}} 

-- <Postfix Expression> ::= <Left Hand Side Expression>
--                        | <Postfix Expression> '++'
--                        | <Postfix Expression> '--'
PostfixExpression : LeftHandSideExpression { $1 {- PostfixExpression -} } 
                  | PostfixExpression '++' {[(AST.JSExpressionPostfix "++" $1)]}
                  | PostfixExpression '--' {[(AST.JSExpressionPostfix "--" $1)]}

-- <Unary Expression> ::= <Postfix Expression>
--                      | 'delete' <Unary Expression>
--                      | 'void' <Unary Expression>
--                      | 'typeof' <Unary Expression>
--                      | '++' <Unary Expression>
--                      | '--' <Unary Expression>
--                      | '+' <Unary Expression>
--                      | '-' <Unary Expression>
--                      | '~' <Unary Expression>
--                      | '!' <Unary Expression>
UnaryExpression : PostfixExpression { $1 {- UnaryExpression -} } 
                | 'delete' UnaryExpression { ((AST.JSUnary "delete "):$2)}
                | 'void'   UnaryExpression { ((AST.JSUnary "void "):$2)}
                | 'typeof' UnaryExpression { ((AST.JSUnary "typeof "):$2)}
                | '++'     UnaryExpression { ((AST.JSUnary "++"):$2) } 
                | '--'     UnaryExpression { ((AST.JSUnary "--"):$2)}
                | '+'      UnaryExpression { ((AST.JSUnary "+"):$2)}
                | '-'      UnaryExpression { ((AST.JSUnary "-"):$2)}
                | '~'      UnaryExpression { ((AST.JSUnary "~"):$2)}
                | '!'      UnaryExpression { ((AST.JSUnary "!"):$2)}


-- <Multiplicative Expression> ::= <Unary Expression>
--                               | <Unary Expression> '*' <Multiplicative Expression> 
--                               | <Unary Expression> '/' <Multiplicative Expression>                               
--                               | <Unary Expression> '%' <Multiplicative Expression> 
MultiplicativeExpression : UnaryExpression { $1 {- MultiplicativeExpression -}} 
                         | UnaryExpression '*' MultiplicativeExpression { [(AST.JSExpressionBinary "*" $1 $3)]}
                         | UnaryExpression '/' MultiplicativeExpression { [(AST.JSExpressionBinary "/" $1 $3)]}
                         | UnaryExpression '%' MultiplicativeExpression { [(AST.JSExpressionBinary "%" $1 $3)]}

-- <Additive Expression> ::= <Additive Expression>'+'<Multiplicative Expression> 
--                         | <Additive Expression>'-'<Multiplicative Expression>  
--                         | <Multiplicative Expression> 
AdditiveExpression : AdditiveExpression '+' MultiplicativeExpression { [(AST.JSExpressionBinary "+" $1 $3)]}
                   | AdditiveExpression '-' MultiplicativeExpression { [(AST.JSExpressionBinary "-" $1 $3)]}
                   | MultiplicativeExpression { $1 {- AdditiveExpression -} } 



-- <Shift Expression> ::= <Shift Expression> '<<' <Additive Expression>
--                      | <Shift Expression> '>>' <Additive Expression>
--                      | <Shift Expression> '>>>' <Additive Expression>
--                      | <Additive Expression>
ShiftExpression : ShiftExpression '<<'  AdditiveExpression { [(AST.JSExpressionBinary "<<" $1 $3)]}
                | ShiftExpression '>>'  AdditiveExpression { [(AST.JSExpressionBinary ">>" $1 $3)]}
                | ShiftExpression '>>>' AdditiveExpression { [(AST.JSExpressionBinary ">>>" $1 $3)]}
                | AdditiveExpression { $1 {- ShiftExpression -}} 

-- <Relational Expression>::= <Shift Expression> 
--                          | <Relational Expression> '<' <Shift Expression> 
--                          | <Relational Expression> '>' <Shift Expression> 
--                          | <Relational Expression> '<=' <Shift Expression> 
--                          | <Relational Expression> '>=' <Shift Expression> 
--                          | <Relational Expression> 'instanceof' <Shift Expression> 
RelationalExpression : ShiftExpression { $1 {- RelationalExpression -}} -- TODO: restore rest
                     | RelationalExpression '<'  ShiftExpression { [(AST.JSExpressionBinary "<" $1 $3)]}
                     | RelationalExpression '>'  ShiftExpression { [(AST.JSExpressionBinary ">" $1 $3)]}
                     | RelationalExpression '<=' ShiftExpression { [(AST.JSExpressionBinary "<=" $1 $3)]}
                     | RelationalExpression '>=' ShiftExpression { [(AST.JSExpressionBinary ">=" $1 $3)]}
                     | RelationalExpression 'instanceof' ShiftExpression { [(AST.JSExpressionBinary " instanceof " $1 $3)]}


-- <Equality Expression> ::= <Relational Expression>
--                         | <Equality Expression> '==' <Relational Expression>
--                         | <Equality Expression> '!=' <Relational Expression>
--                         | <Equality Expression> '===' <Relational Expression>
--                         | <Equality Expression> '!==' <Relational Expression>
EqualityExpression : RelationalExpression { $1 {- EqualityExpression -} } 
                   | EqualityExpression '=='  RelationalExpression { [(AST.JSExpressionBinary "==" $1 $3)]}
                   | EqualityExpression '!='  RelationalExpression { [(AST.JSExpressionBinary "!=" $1 $3)]}
                   | EqualityExpression '===' RelationalExpression { [(AST.JSExpressionBinary "===" $1 $3)]}
                   | EqualityExpression '!==' RelationalExpression { [(AST.JSExpressionBinary "!==" $1 $3)]}


-- <Bitwise And Expression> ::= <Equality Expression>
--                            | <Bitwise And Expression> '&' <Equality Expression>
BitwiseAndExpression : EqualityExpression { $1 {- BitwiseAndExpression -} } -- TODO: restore rest
                     | BitwiseAndExpression '&' EqualityExpression { [(AST.JSExpressionBinary "&" $1 $3)]}

-- <Bitwise XOr Expression> ::= <Bitwise And Expression>
--                            | <Bitwise XOr Expression> '^' <Bitwise And Expression>
BitwiseXOrExpression : BitwiseAndExpression { $1 {- BitwiseXOrExpression -} } -- TODO: restore rest
                     | BitwiseXOrExpression '^' BitwiseAndExpression { [(AST.JSExpressionBinary "^" $1 $3)]}

-- <Bitwise Or Expression> ::= <Bitwise XOr Expression>
--                           | <Bitwise Or Expression> '|' <Bitwise XOr Expression>
BitwiseOrExpression : BitwiseXOrExpression { $1 {- BitwiseOrExpression -} } -- TODO: restore rest
                    | BitwiseOrExpression '|' BitwiseXOrExpression { [(AST.JSExpressionBinary "|" $1 $3)]}

-- <Logical And Expression> ::= <Bitwise Or Expression>
--                            | <Logical And Expression> '&&' <Bitwise Or Expression>
LogicalAndExpression : BitwiseOrExpression { $1 {- LogicalAndExpression -} } -- TODO: restore rest
                     | LogicalAndExpression '&&' BitwiseOrExpression { [(AST.JSExpressionBinary "&&" $1 $3)]}

-- <Logical Or Expression> ::= <Logical And Expression>
--                           | <Logical Or Expression> '||' <Logical And Expression>
LogicalOrExpression : LogicalAndExpression { $1 {- LogicalOrExpression -} } 
                    | LogicalOrExpression '||' LogicalAndExpression { [(AST.JSExpressionBinary "||" $1 $3)]}

-- <Conditional Expression> ::= <Logical Or Expression> 
--                            | <Logical Or Expression> '?' <Assignment Expression> ':' <Assignment Expression>
ConditionalExpression : LogicalOrExpression { $1 {- ConditionalExpression -} }
                    | LogicalOrExpression '?' AssignmentExpression ':' AssignmentExpression 
                      { [AST.JSExpressionTernary $1 $3 $5] } 
                    
  
-- <Assignment Expression> ::= <Conditional Expression>
--                           | <Left Hand Side Expression> <Assignment Operator> <Assignment Expression> 
AssignmentExpression :: { [AST.JSNode] }
AssignmentExpression : ConditionalExpression { $1 {- AssignmentExpression -}} 
                     | LeftHandSideExpression AssignmentOperator AssignmentExpression 
                       { [(AST.JSElement "assignmentExpression" ($1++[$2]++$3))] }
                       
-- <Assignment Operator> ::= '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '>>>=' | '&=' | '^=' | '|='
AssignmentOperator : 'assign' { AST.JSOperator (token_literal $1) }
                   | '='      { AST.JSOperator "=" }

-- <Expression> ::= <Assignment Expression>
--                | <Expression> ',' <Assignment Expression>
Expression : AssignmentExpression { AST.JSExpression $1 {- Expression -} } -- TODO: restore rest
           -- | Expression ',' AssignmentExpression

ExpressionOpt : Expression { [$1] {- ExpressionOpt -}}
              |            { []   {- ExpressionOpt -}}
                           
-- <Statement> ::= <Block>
--               | <Variable Statement>
--               | <Empty Statement>
--               | <If Statement>
--               | <If Else Statement>
--               | <Iteration Statement>
--               | <Continue Statement>
--               | <Break Statement>
--               | <Return Statement>
--               | <With Statement>
--               | <Labelled Statement>
--               | <Switch Statement>
--               | <Throw Statement>
--               | <Try Statement>
--               | <Expression> 
Statement : Block              { $1 {- Statement1 -}}
          | VariableStatement  { $1 {- Statement2 -}}
          | EmptyStatement     { $1 {- Statement3 -}}
          | IfStatement        { $1 {- Statement4 -}}
          | IfElseStatement    { $1 {- Statement5 -}}
          | IterationStatement { $1 {- Statement6 -}}
          | ContinueStatement  { $1 {- Statement7 -}}
          | BreakStatement     { $1 {- Statement8 -}}
          | ReturnStatement    { $1 {- Statement9 -}}
          | WithStatement      { $1 {- Statement10 -}}
          | LabelledStatement  { $1 {- Statement11 -}}
          | SwitchStatement    { $1 {- Statement12 -}}
          | ThrowStatement     { $1 {- Statement13 -}}
          | TryStatement       { $1 {- Statement14 -}}
          | Expression         { $1 {- Statement15 -}}

Block : '{' '}'               { (AST.JSBlock (AST.JSStatementList [])) }
      | '{' StatementList '}' { (AST.JSBlock (AST.JSStatementList [$2])) }

StatementList :: { AST.JSNode }
StatementList : Statement               { (AST.JSStatementList [$1]) }
              | StatementList Statement { (combineStatements $1 $2) }

-- <Variable Statement> ::= var <Variable Declaration List> ';'
VariableStatement : 'var'   VariableDeclarationList ';' { AST.JSVariables "var" $2 }
                  | 'const' VariableDeclarationList ';' { AST.JSVariables "const" $2 }

-- <Variable Declaration List> ::= <Variable Declaration>
--                               | <Variable Declaration List> ',' <Variable Declaration>
VariableDeclarationList :: { [AST.JSNode] }
VariableDeclarationList : VariableDeclaration { [$1] {- VariableDeclarationList -}}
                        | VariableDeclarationList ',' VariableDeclaration { ($1 ++ [$3]) {- VariableDeclarationList -}}

VariableDeclaration :: { AST.JSNode }
VariableDeclaration : Identifier              { (AST.JSVarDecl $1 [])}
                    | Identifier Initializer  { (AST.JSVarDecl $1 $2)}

-- <Initializer> ::= '=' <Assignment Expression>
Initializer : '=' AssignmentExpression { $2 {- Initializer -}}

EmptyStatement : ';' { (AST.JSLiteral ";") }

-- <If Statement> ::= 'if' '(' <Expression> ')' <Statement> 
IfStatement : 'if' '(' Expression ')' Statement { (AST.JSIf $3 $5) }

-- <If Else Statement> ::= 'if' '(' <Expression> ')' <Statement> 'else' <Statement>
IfElseStatement : 'if' '(' Expression ')' Statement 'else' Statement { (AST.JSIfElse $3 $5 $7) }

-- <Iteration Statement> ::= 'do' <Statement> 'while' '(' <Expression> ')' ';'
--                         | 'while' '(' <Expression> ')' <Statement> 
--                         | 'for' '(' <Expression> ';' <Expression> ';' <Expression> ')' <Statement> 
--                         | 'for' '(' 'var' <Variable Declaration List> ';' <Expression> ';' <Expression> ')' <Statement> 
--                         | 'for' '(' <Left Hand Side Expression> in <Expression> ')' <Statement> 
--                         | 'for' '(' 'var' <Variable Declaration> in <Expression> ')' <Statement> 
IterationStatement :: { AST.JSNode }
IterationStatement : 'do' Statement 'while' '(' Expression ')' ';' { (AST.JSDoWhile $2 $5 (AST.JSLiteral ";")) } -- TODO: sort out autosemi
                   | 'while' '(' Expression ')' Statement { (AST.JSWhile $3 $5) }
                   | 'for' '(' ExpressionOpt ';' ExpressionOpt ';' ExpressionOpt ')' Statement { (AST.JSFor $3 $5 $7 $9) }
                   | 'for' '(' 'var' VariableDeclarationList ';' ExpressionOpt ';' ExpressionOpt ')' Statement 
                     { (AST.JSForVar $4 $6 $8 $10) }
                   | 'for' '(' LeftHandSideExpression 'in' Expression ')' Statement 
                     { (AST.JSForIn $3 $5 $7) }
                   | 'for' '(' 'var' VariableDeclaration 'in' Expression ')' Statement
                     { (AST.JSForVarIn $4 $6 $8) }

-- <Continue Statement> ::= 'continue' ';'
--                        | 'continue' Identifier ';'
ContinueStatement : 'continue' ';'             { (AST.JSContinue [AST.JSLiteral ";"]) } 
                  | 'continue' Identifier ';'  { (AST.JSContinue [$2,AST.JSLiteral ";"]) } 

-- <Break Statement> ::= 'break' ';'
--                        | 'break' Identifier ';'
BreakStatement : 'break' ';'             { (AST.JSBreak [] [AST.JSLiteral ";"]) } 
               | 'break' Identifier ';'  { (AST.JSBreak [$2] [AST.JSLiteral ";"]) } 

-- <Return Statement> ::= 'return' ';'
--                        | 'return' <Expression> ';'
ReturnStatement : 'return' ';'             { (AST.JSReturn [AST.JSLiteral ";"]) } 
                | 'return' Expression ';'  { (AST.JSReturn [$2,AST.JSLiteral ";"]) } 

-- <With Statement> ::= 'with' '(' <Expression> ')' <Statement> ';'
WithStatement : 'with' '(' Expression ')' Statement ';'  { (AST.JSWith $3 [$5,AST.JSLiteral ";"]) }

-- <Switch Statement> ::= 'switch' '(' <Expression> ')' <Case Block>  
SwitchStatement : 'switch' '(' Expression ')' CaseBlock { (AST.JSSwitch $3 $5) } 

-- <Case Block> ::= '{' '}'
--                | '{' <Case Clauses> '}'
--                | '{' <Case Clauses> <Default Clause> '}'
--                | '{' <Case Clauses> <Default Clause> <Case Clauses> '}'
--                | '{' <Default Clause> <Case Clauses> '}'
--                | '{' <Default Clause> '}'
CaseBlock :: { [AST.JSNode] }
CaseBlock : '{' '}'                                       { [] }
          | '{' CaseClauses '}'                           { $2            {- CaseBlock2 -}}
          | '{' CaseClauses DefaultClause '}'             { ($2++[$3])    {- CaseBlock3 -}}
          | '{' CaseClauses DefaultClause CaseClauses '}' { ($2++($3:$4)) {- CaseBlock4 -}}
          | '{' DefaultClause CaseClauses '}'             { ($2:$3)       {- CaseBlock5 -}}
          | '{' DefaultClause '}'                         { [$2]          {- CaseBlock6 -}}

-- <Case Clauses> ::= <Case Clause>
--                  | <Case Clauses> <Case Clause>
CaseClauses :: { [AST.JSNode] }
CaseClauses : CaseClause               { [$1] {- CaseClauses1 -}}
            | CaseClauses CaseClause   { ($1++[$2]) {- CaseClauses2 -}}

-- <Case Clause> ::= 'case' <Expression> ':' <Statement List>
--                 | 'case' <Expression> ':'
CaseClause :: { AST.JSNode }
CaseClause : 'case' Expression ':' StatementList  { (AST.JSCase $2 $4) }
           | 'case' Expression ':'                { (AST.JSCase $2 (AST.JSStatementList [])) }

-- <Default Clause> ::= 'default' ':' 
--                    | 'default' ':' <Statement List>
DefaultClause :: { AST.JSNode }
DefaultClause : 'default' ':'                { (AST.JSDefault (AST.JSStatementList [])) }
              | 'default' ':' StatementList  { (AST.JSDefault $3) }

-- <Labelled Statement> ::= Identifier ':' <Statement> 
LabelledStatement : Identifier ':' Statement { (AST.JSLabelled $1 $3) }

-- <Throw Statement> ::= 'throw' <Expression>
ThrowStatement : 'throw' Expression { (AST.JSThrow $2) }

-- <Try Statement> ::= 'try' <Block> <Catch>
--                   | 'try' <Block> <Finally>
--                   | 'try' <Block> <Catch> <Finally>
-- TODO: add syntax extensions
TryStatement : 'try' Block Catch         { (AST.JSTry $2 [$3])    {- TryStatement1 -} }
             | 'try' Block Finally       { (AST.JSTry $2 [$3])    {- TryStatement2 -} }
             | 'try' Block Catch Finally { (AST.JSTry $2 [$3,$4]) {- TryStatement3 -} }

-- <Catch> ::= 'catch' '(' Identifier ')' <Block>
-- TODO: syntax extensions
Catch : 'catch' '(' Identifier ')' Block { (AST.JSCatch $3 [] $5) }

-- <Finally> ::= 'finally' <Block>
Finally : 'finally' Block { (AST.JSFinally $2) }

-- <Function Declaration> ::= 'function' Identifier '(' <Formal Parameter List> ')' '{' <Function Body> '}'
--                          | 'function' Identifier '(' ')' '{' <Function Body> '}'

-- <Function Expression> ::= 'function' '(' ')' '{' <Function Body> '}'
--                         | 'function' '(' <Formal Parameter List> ')' '{' <Function Body> '}'
FunctionExpression :: { AST.JSNode }
FunctionExpression : 'function' '(' ')' '{' FunctionBody '}'                      { (AST.JSFunctionExpression [] $5) }
                   | 'function' '(' FormalParameterList ')' '{' FunctionBody '}'  { (AST.JSFunctionExpression $3 $6) }


-- <Formal Parameter List> ::= Identifier
--                           | <Formal Parameter List> ',' Identifier
FormalParameterList :: { [AST.JSNode] }
FormalParameterList : Identifier                          { [$1] {- FormalParameterList -}}
                    | FormalParameterList ',' Identifier  { ($1++[$3]) }

-- <Function Body> ::= <Source Elements>
--                   | 
FunctionBody :: { AST.JSNode }
FunctionBody : SourceElements { (AST.JSFunctionBody [$1]) }
             |                { (AST.JSFunctionBody []) } 

-- <Program> ::= <Source Elements>
Program : SourceElements { $1 {- Program -}}

-- <Source Elements> ::= <Source Element>
--                     | <Source Elements>  <Source Element>
SourceElements :: { AST.JSNode }
SourceElements : SourceElement                { (AST.JSSourceElements [$1]) }
               | SourceElements SourceElement { (combineSourceElements $1 $2) }

-- <Source Element> ::= <Statement>
--                    | <Function Declaration>
SourceElement :: { AST.JSNode }
SourceElement : Statement            { $1 {- SourceElement1 -} }
              -- | FunctionDeclaration  { $1 {- SourceElement2 -} } -- TODO: restore

{
combineSourceElements :: AST.JSNode -> AST.JSNode -> AST.JSNode
combineSourceElements (AST.JSSourceElements xs) x = (AST.JSSourceElements (xs++[x]) )

combineStatements :: AST.JSNode -> AST.JSNode -> AST.JSNode
combineStatements (AST.JSStatementList xs) (AST.JSStatementList ys) = (AST.JSStatementList (xs++ys) )
combineStatements (AST.JSStatementList xs) y = (AST.JSStatementList (xs++[y]) )

parseError :: Token -> P a 
parseError = throwError . UnexpectedToken 

{-
flattenExpression :: [[AST.JSNode]] -> [AST.JSNode]
flattenExpression val = flatten $ intersperse litComma val
                        where
                          litComma :: [AST.JSNode]
                          litComma = [(AST.JSLiteral ",")]
-}

}

-- Set emacs mode
-- Local Variables: 
-- mode:haskell
-- End:             
