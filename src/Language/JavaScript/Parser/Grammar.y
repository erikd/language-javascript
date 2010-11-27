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
     -- '='	{ AssignToken {} }
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
PrimaryExpression : 'this'     { AST.JSLiteral "this" }
                  | Identifier { $1 }
                  | Literal    { $1 }
                  | ArrayLiteral { $1 }
                  -- | ObjectLiteral
                  -- | '(' Expression ')'
                  | RegularExpressionLiteral { $1 }
                  
Identifier : 'ident' { AST.JSIdentifier (token_literal $1) }

-- <Array Literal> ::= '[' ']'
--                   | '[' <Elision> ']'
--                   | '[' <Element List> ']'
--                   | '[' <Element List> ',' <Elision> ']'
ArrayLiteral : '[' ']' { AST.JSArrayLiteral [] }
             | '[' Elision ']' { AST.JSArrayLiteral $2 }
             -- | '[' <Element List> ']' --TODO : uncomment this
             -- | '[' <Element List> ',' <Elision> ']'

-- <Elision> ::= ','
--             | <Elision> ','
Elision :  ',' { [(AST.JSElision [])] }
        | Elision ',' { $1 ++ [(AST.JSElision [])] }


-- <Element List> ::= <Elision> <Assignment Expression>
--                  | <Element List> ',' <Elision>  <Assignment Expression>
--                  | <Element List> ',' <Assignment Expression>
--                  | <Assignment Expression>
{-
ElementList : Elision AssignmentExpression                    {return [(AST.JSElementList ($1++$2)) ] }
            | ElementList ',' Elision  AssignmentExpression   {return [(AST.JSElementList ($1++$3++$4))] }
            | ElementList ',' AssignmentExpression            {return [(AST.JSElementList ($1++$3))] }
            | AssignmentExpression                            { $1 }
-}

-- <Object Literal> ::= '{' <Property Name and Value List> '}'

-- <Property Name and Value List> ::= <Property Name> ':' <Assignment Expression>
--                                  | <Property Name and Value List> ',' <Property Name> ':' <Assignment Expression>

-- <Property Name> ::= Identifier
--                   | StringLiteral
--                   | <Numeric Literal>

-- <Member Expression > ::= <Primary Expression>
--                        | <Function Expression>
--                        | <Member Expression> '[' <Expression> ']'
--                        | <Member Expression> '.' Identifier
--                        | 'new' <Member Expression> <Arguments>
MemberExpression : PrimaryExpression { $1 {- MemberExpression -}} -- TODO : uncomment rest, restore $1
                 -- | FunctionExpression
                 -- | MemberExpression '[' Expression ']'
                 -- | MemberExpression '.' Identifier { [AST.JSMemberDot ($1++[$3])] } 
                 -- | 'new' MemberExpression Arguments { (((AST.JSLiteral "new "):$2)++[$3])}

-- <New Expression> ::= <Member Expression>
--                    | new <New Expression>
NewExpression : MemberExpression {$1 {- NewExpression -}} -- TODO: uncomment next
              -- | 'new' NewExpression { (AST.JSLiteral "new"):$2 }

-- <Call Expression> ::= <Member Expression> <Arguments>
--                     | <Call Expression> <Arguments> 
--                     | <Call Expression> '[' <Expression> ']'
--                     | <Call Expression> '.' Identifier
{-
CallExpression : MemberExpression Arguments { $1++[$2] {- CallExpression -} } 
               -- | <Call Expression> <Arguments> -- TODO: finish this
               -- | <Call Expression> '[' <Expression> ']'
               -- | <Call Expression> '.' Identifier
-}

-- <Arguments> ::= '(' ')'
--               | '(' <Argument List> ')'
Arguments : '(' ')' { AST.JSArguments [] } -- TODO: restore rest
          -- | '(' ArgumentList ')' { AST.JSArguments $2 }

-- <Argument List> ::= <Assignment Expression>
--                   | <Argument List> ',' <Assignment Expression>
{-
ArgumentList : AssignmentExpression { $1 {- ArgumentList -}}
             | ArgumentList ',' AssignmentExpression { $1++$3 }
-}

-- <Left Hand Side Expression> ::= <New Expression> 
--                               | <Call Expression>
LeftHandSideExpression : NewExpression  { $1 {- LeftHandSideExpression1 -}}
                       -- | CallExpression { $1 {- LeftHandSideExpression12 -}} -- TODO: restore rest

-- <Postfix Expression> ::= <Left Hand Side Expression>
--                        | <Postfix Expression> '++'
--                        | <Postfix Expression> '--'
PostfixExpression : LeftHandSideExpression { $1 {- PostfixExpression -} } 
{- TODO: restore this
                  | PostfixExpression '++' {[(AST.JSExpressionPostfix "++" $1)]}
                  | PostfixExpression '--' {[(AST.JSExpressionPostfix "--" $1)]}
-}
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
UnaryExpression : PostfixExpression { $1 {- UnaryExpression -} } -- TODO: uncomment the rest
                {-
                | 'delete' UnaryExpression { return ((AST.JSUnary "delete "):$2)}
                | 'void'   UnaryExpression { return ((AST.JSUnary "void "):$2)}
                | 'typeof' UnaryExpression { return ((AST.JSUnary "typeof "):$2)}
                | '++'     UnaryExpression { return ((AST.JSUnary "++"):$2)}
                | '--'     UnaryExpression { return ((AST.JSUnary "--"):$2)}
                | '+'      UnaryExpression { return ((AST.JSUnary "+"):$2)}
                | '-'      UnaryExpression { return ((AST.JSUnary "-"):$2)}
                | '~'      UnaryExpression { return ((AST.JSUnary "~"):$2)}
                | '!'      UnaryExpression { return ((AST.JSUnary "!"):$2)}
-}

-- <Multiplicative Expression> ::= <Unary Expression>
--                               | <Unary Expression> '*' <Multiplicative Expression> 
--                               | <Unary Expression> '/' <Multiplicative Expression>                               
--                               | <Unary Expression> '%' <Multiplicative Expression> 
MultiplicativeExpression : UnaryExpression { $1 {- MultiplicativeExpression -}} -- TODO : restore next
                         -- | UnaryExpression '*' MultiplicativeExpression { [(AST.JSExpressionBinary "*" $1 $3)]}
                         -- | UnaryExpression '/' MultiplicativeExpression { [(AST.JSExpressionBinary "/" $1 $3)]}
                         -- | UnaryExpression '%' MultiplicativeExpression { [(AST.JSExpressionBinary "%" $1 $3)]}

-- <Additive Expression> ::= <Additive Expression>'+'<Multiplicative Expression> 
--                         | <Additive Expression>'-'<Multiplicative Expression>  
--                         | <Multiplicative Expression> 
AdditiveExpression : {-AdditiveExpression '+' MultiplicativeExpression { [(AST.JSExpressionBinary "+" $1 $3)]}
                   | AdditiveExpression '-' MultiplicativeExpression { [(AST.JSExpressionBinary "-" $1 $3)]}
                   | -}MultiplicativeExpression { $1 {- AdditiveExpression -} } -- TODO: restore rest



-- <Shift Expression> ::= <Shift Expression> '<<' <Additive Expression>
--                      | <Shift Expression> '>>' <Additive Expression>
--                      | <Shift Expression> '>>>' <Additive Expression>
--                      | <Additive Expression>
ShiftExpression : {-ShiftExpression '<<'  AdditiveExpression { [(AST.JSExpressionBinary "<<" $1 $3)]}
                | ShiftExpression '>>'  AdditiveExpression { [(AST.JSExpressionBinary ">>" $1 $3)]}
                | ShiftExpression '>>>' AdditiveExpression { [(AST.JSExpressionBinary ">>>" $1 $3)]}
                | -} AdditiveExpression { $1 {- ShiftExpression -}} -- TODO: restore rest

-- <Relational Expression>::= <Shift Expression> 
--                          | <Relational Expression> '<' <Shift Expression> 
--                          | <Relational Expression> '>' <Shift Expression> 
--                          | <Relational Expression> '<=' <Shift Expression> 
--                          | <Relational Expression> '>=' <Shift Expression> 
--                          | <Relational Expression> 'instanceof' <Shift Expression> 
RelationalExpression : ShiftExpression { $1 {- RelationalExpression -}} -- TODO: restore rest
{-
                     | RelationalExpression '<'  ShiftExpression { [(AST.JSExpressionBinary "<" $1 $3)]}
                     | RelationalExpression '>'  ShiftExpression { [(AST.JSExpressionBinary ">" $1 $3)]}
                     | RelationalExpression '<=' ShiftExpression { [(AST.JSExpressionBinary "<=" $1 $3)]}
                     | RelationalExpression '>=' ShiftExpression { [(AST.JSExpressionBinary ">=" $1 $3)]}
                     | RelationalExpression 'instanceof' ShiftExpression { [(AST.JSExpressionBinary " instanceof " $1 $3)]}
-}

-- <Equality Expression> ::= <Relational Expression>
--                         | <Equality Expression> '==' <Relational Expression>
--                         | <Equality Expression> '!=' <Relational Expression>
--                         | <Equality Expression> '===' <Relational Expression>
--                         | <Equality Expression> '!==' <Relational Expression>
EqualityExpression : RelationalExpression { $1 {- EqualityExpression -} } -- TODO: restore rest
{-
                   | EqualityExpression '=='  RelationalExpression { [(AST.JSExpressionBinary "==" $1 $3)]}
                   | EqualityExpression '!='  RelationalExpression { [(AST.JSExpressionBinary "!=" $1 $3)]}
                   | EqualityExpression '===' RelationalExpression { [(AST.JSExpressionBinary "===" $1 $3)]}
                   | EqualityExpression '!==' RelationalExpression { [(AST.JSExpressionBinary "!==" $1 $3)]}
-}

-- <Bitwise And Expression> ::= <Equality Expression>
--                            | <Bitwise And Expression> '&' <Equality Expression>
BitwiseAndExpression : EqualityExpression { $1 {- BitwiseAndExpression -} } -- TODO: restore rest
                     -- | BitwiseAndExpression '&' EqualityExpression { [(AST.JSExpressionBinary "&" $1 $3)]}

-- <Bitwise XOr Expression> ::= <Bitwise And Expression>
--                            | <Bitwise XOr Expression> '^' <Bitwise And Expression>
BitwiseXOrExpression : BitwiseAndExpression { $1 {- BitwiseXOrExpression -} } -- TODO: restore rest
                     -- | BitwiseXOrExpression '^' BitwiseAndExpression { [(AST.JSExpressionBinary "^" $1 $3)]}

-- <Bitwise Or Expression> ::= <Bitwise XOr Expression>
--                           | <Bitwise Or Expression> '|' <Bitwise XOr Expression>
BitwiseOrExpression : BitwiseXOrExpression { $1 {- BitwiseOrExpression -} } -- TODO: restore rest
                    -- | BitwiseOrExpression '|' BitwiseXOrExpression { [(AST.JSExpressionBinary "|" $1 $3)]}

-- <Logical And Expression> ::= <Bitwise Or Expression>
--                            | <Logical And Expression> '&&' <Bitwise Or Expression>
LogicalAndExpression : BitwiseOrExpression { $1 {- LogicalAndExpression -} } -- TODO: restore rest
                     -- | LogicalAndExpression '&&' BitwiseOrExpression { [(AST.JSExpressionBinary "&&" $1 $3)]}

-- <Logical Or Expression> ::= <Logical And Expression>
--                           | <Logical Or Expression> '||' <Logical And Expression>
LogicalOrExpression : LogicalAndExpression { $1 {- LogicalOrExpression -} } -- TODO: restore rest
                    -- | LogicalOrExpression '||' LogicalAndExpression { [(AST.JSExpressionBinary "||" $1 $3)]}

-- <Conditional Expression> ::= <Logical Or Expression> 
--                            | <Logical Or Expression> '?' <Assignment Expression> ':' <Assignment Expression>
ConditionalExpression : LogicalOrExpression { $1 {- ConditionalExpression -} }
                    {- TODO: Restore rest
                    | LogicalOrExpression '?' AssignmentExpression ':' AssignmentExpression 
                      { AST.JSExpressionTernary $1 $3 $5 } -}

-- <Assignment Expression> ::= <Conditional Expression>
--                           | <Left Hand Side Expression> <Assignment Operator> <Assignment Expression> 
AssignmentExpression : ConditionalExpression { $1 {- AssignmentExpression -}} -- TODO: restore rest
                     -- | LeftHandSideExpression AssignmentOperator AssignmentExpression
                     --   { [(AST.JSElement "assignmentExpression" ($1++$2++$3))]}
                       
-- <Assignment Operator> ::= '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '>>>=' | '&=' | '^=' | '|='
AssignmentOperator : 'assign' { AST.JSOperator (token_literal $1) }

-- <Expression> ::= <Assignment Expression>
--                | <Expression> ',' <Assignment Expression>
Expression : AssignmentExpression { $1 {- Expression -} } -- TODO: restore rest
           -- | Expression ',' AssignmentExpression

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
Statement : {- Block
          | <Variable Statement>
          | <Empty Statement>
          | <If Statement>
          | <If Else Statement>
          | <Iteration Statement>
          | <Continue Statement>
          | <Break Statement>
          | <Return Statement>
          | <With Statement>
          | <Labelled Statement>
          | <Switch Statement>
          | <Throw Statement>
          | <Try Statement>
          | -} Expression { $1 {- Statement -}}

-- <Block > ::= '{' '}'
--            | '{' <Statement List> '}'

-- <Statement List> ::= <Statement>
--                    | <Statement List> <Statement>

-- <Variable Statement> ::= var <Variable Declaration List> ';'
-- <Variable Declaration List> ::= <Variable Declaration>
--                               | <Variable Declaration List> ',' <Variable Declaration>

-- <Variable Declaration> ::= Identifier
--                          | Identifier <Initializer>

-- <Initializer> ::= '=' <Assignment Expression>

-- <Empty Statement> ::= ';'

-- <If Statement> ::= 'if' '(' <Expression> ')' <Statement> 
-- <If Else Statement> ::= 'if' '(' <Expression> ')' <Statement> 'else' <Statement>

-- <Iteration Statement> ::= 'do' <Statement> 'while' '(' <Expression> ')' ';'
--                         | 'while' '(' <Expression> ')' <Statement> 
--                         | 'for' '(' <Expression> ';' <Expression> ';' <Expression> ')' <Statement> 
--                         | 'for' '(' 'var' <Variable Declaration List> ';' <Expression> ';' <Expression> ')' <Statement> 
--                         | 'for' '(' <Left Hand Side Expression> in <Expression> ')' <Statement> 
--                         | 'for' '(' 'var' <Variable Declaration> in <Expression> ')' <Statement> 

-- <Continue Statement> ::= 'continue' ';'
--                        | 'continue' Identifier ';'

-- <Break Statement> ::= 'break' ';'
--                        | 'break' Identifier ';'

-- <Return Statement> ::= 'return' ';'
--                        | 'return' <Expression> ';'

-- <With Statement> ::= 'with' '(' <Expression> ')' <Statement> ';'

-- <Switch Statement> ::= 'switch' '(' <Expression> ')' <Case Block>  

-- <Case Block> ::= '{' '}'
--                | '{' <Case Clauses> '}'
--                | '{' <Case Clauses> <Default Clause> '}'
--                | '{' <Case Clauses> <Default Clause> <Case Clauses> '}'
--                | '{' <Default Clause> <Case Clauses> '}'
--                | '{' <Default Clause> '}'

-- <Case Clauses> ::= <Case Clause>
--                  | <Case Clauses> <Case Clause>

-- <Case Clause> ::= 'case' <Expression> ':' <Statement List>
--                 | 'case' <Expression> ':'

-- <Default Clause> ::= 'default' ':' 
--                    | 'default' ':' <Statement List>

-- <Labelled Statement> ::= Identifier ':' <Statement> 

-- <Throw Statement> ::= 'throw' <Expression>

-- <Try Statement> ::= 'try' <Block> <Catch>
--                   | 'try' <Block> <Finally>
--                   | 'try' <Block> <Catch> <Finally>

-- <Catch> ::= 'catch' '(' Identifier ')' <Block>

-- <Finally> ::= 'finally' <Block>

-- <Function Declaration> ::= 'function' Identifier '(' <Formal Parameter List> ')' '{' <Function Body> '}'
--                          | 'function' Identifier '(' ')' '{' <Function Body> '}'

-- <Function Expression> ::= 'function' '(' ')' '{' <Function Body> '}'
--                         | 'function' '(' <Formal Parameter List> ')' '{' <Function Body> '}'


-- <Formal Parameter List> ::= Identifier
--                           | <Formal Parameter List> ',' Identifier

-- <Function Body> ::= <Source Elements>
--                   | 

-- <Program> ::= <Source Elements>

-- <Source Elements> ::= <Source Element>
--                     | <Source Elements>  <Source Element>

-- <Source Element> ::= <Statement>
--                    | <Function Declaration>

{

parseError :: Token -> P a 
parseError = throwError . UnexpectedToken 

}

-- Set emacs mode
-- Local Variables: 
-- mode:haskell
-- End:             
