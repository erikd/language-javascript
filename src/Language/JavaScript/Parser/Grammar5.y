{
module Language.JavaScript.Parser.Grammar5 (
    parseProgram
  , parseLiteral
  , parsePrimaryExpression
  , parseStatement
  ) where

import Control.Monad.Error.Class (throwError)
import Data.Char
import Language.JavaScript.Parser.Lexer
import Language.JavaScript.Parser.ParserMonad
import Language.JavaScript.Parser.SrcLocation
import qualified Language.JavaScript.Parser.AST as AST

}

-- The name of the generated function to be exported from the module
%name parseProgram           Program
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
     'get'        { GetToken {} }
     'if'         { IfToken {} }
     'in'         { InToken {} }
     'instanceof' { InstanceofToken {} }
     'new'        { NewToken {} }
     'null'       { NullToken {} }
     'return'     { ReturnToken {} }
     'set'        { SetToken {} }
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

     'future'     { FutureToken {} }


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



-- ---------------------------------------------------------------------
-- Sort out automatically inserted semi-colons

AutoSemi :: { AST.JSNode }
AutoSemi : ';' { AST.NS (AST.JSLiteral ";") (ss $1)}
         |     { AST.NS (AST.JSLiteral "") SpanEmpty }
           
-- ---------------------------------------------------------------------

-- Literal ::                                                                See 7.8
--         NullLiteral
--         BooleanLiteral
--         NumericLiteral
--         StringLiteral
Literal :: { AST.JSNode }
Literal : NullLiteral     {$1}
        | BooleanLiteral  {$1}
        | NumericLiteral  {$1}
        | StringLiteral   {$1}
        | RegularExpressionLiteral {$1}  

NullLiteral :: { AST.JSNode }
NullLiteral : 'null' { AST.NS (AST.JSLiteral "null") (ss $1) }

BooleanLiteral :: { AST.JSNode }
BooleanLiteral : 'true'  { AST.NS (AST.JSLiteral "true")  (ss $1)}
               | 'false' { AST.NS (AST.JSLiteral "false") (ss $1)}

-- <Numeric Literal> ::= DecimalLiteral
--                     | HexIntegerLiteral
NumericLiteral :: { AST.JSNode }
NumericLiteral : 'decimal'    { AST.NS (AST.JSDecimal (token_literal $1)) (ss $1)}
               | 'hexinteger' { AST.NS (AST.JSHexInteger (token_literal $1)) (ss $1)}

StringLiteral :: { AST.JSNode }
StringLiteral : 'string'  { AST.NS (AST.JSStringLiteral (token_delimiter $1) (token_literal $1)) (ss $1)}

-- <Regular Expression Literal> ::= RegExp 
RegularExpressionLiteral :: { AST.JSNode }
RegularExpressionLiteral : 'regex' { AST.NS (AST.JSRegEx (token_literal $1)) (ss $1)}

-- PrimaryExpression :                                                   See 11.1
--        this
--        Identifier
--        Literal
--        ArrayLiteral
--        ObjectLiteral
--        ( Expression )
PrimaryExpression :: { AST.JSNode }
PrimaryExpression : 'this'                   { AST.NS (AST.JSLiteral "this") (ss $1) }
                  | Identifier               { $1 {- PrimaryExpression1 -}}
                  | Literal                  { $1 {- PrimaryExpression2 -}}
                  | ArrayLiteral             { $1 {- PrimaryExpression3 -}}
                  | ObjectLiteral            { $1 {- PrimaryExpression4 -}}
                  | '(' Expression ')'       { AST.NS (AST.JSExpressionParen $2) (ss $1) }
                  
-- Identifier ::                                                            See 7.6
--         IdentifierName but not ReservedWord
-- IdentifierName ::                                                        See 7.6
--         IdentifierStart
--         IdentifierName IdentifierPart
Identifier :: { AST.JSNode }
Identifier : 'ident' { AST.NS (AST.JSIdentifier (token_literal $1)) (ss $1) }
           | 'get'   { AST.NS (AST.JSIdentifier "get") (ss $1) }  
           | 'set'   { AST.NS (AST.JSIdentifier "set") (ss $1) }  

-- TODO: make this include any reserved word too, including future ones
IdentifierName :: { AST.JSNode }
IdentifierName : Identifier {$1}
             | 'break'      { AST.NS (AST.JSIdentifier "break") (ss $1) }  
             | 'case'       { AST.NS (AST.JSIdentifier "case") (ss $1) }
             | 'catch'      { AST.NS (AST.JSIdentifier "catch") (ss $1) }
             | 'const'      { AST.NS (AST.JSIdentifier "const") (ss $1) }
             | 'continue'   { AST.NS (AST.JSIdentifier "continue") (ss $1) }
             | 'debugger'   { AST.NS (AST.JSIdentifier "debugger") (ss $1) }
             | 'default'    { AST.NS (AST.JSIdentifier "default") (ss $1) }
             | 'delete'     { AST.NS (AST.JSIdentifier "delete") (ss $1) }
             | 'do'         { AST.NS (AST.JSIdentifier "do") (ss $1) }
             | 'else'       { AST.NS (AST.JSIdentifier "else") (ss $1) }
             | 'enum'       { AST.NS (AST.JSIdentifier "enum") (ss $1) }
             | 'false'      { AST.NS (AST.JSIdentifier "false") (ss $1) }
             | 'finally'    { AST.NS (AST.JSIdentifier "finally") (ss $1) }
             | 'for'        { AST.NS (AST.JSIdentifier "for")  (ss $1)}
             | 'function'   { AST.NS (AST.JSIdentifier "function") (ss $1) }
             | 'get'        { AST.NS (AST.JSIdentifier "get") (ss $1) }
             | 'if'         { AST.NS (AST.JSIdentifier "if") (ss $1) }
             | 'in'         { AST.NS (AST.JSIdentifier "in") (ss $1) }
             | 'instanceof' { AST.NS (AST.JSIdentifier "instanceof") (ss $1) }
             | 'new'        { AST.NS (AST.JSIdentifier "new") (ss $1) }
             | 'null'       { AST.NS (AST.JSIdentifier "null") (ss $1) }
             | 'return'     { AST.NS (AST.JSIdentifier "return") (ss $1) }
             | 'set'        { AST.NS (AST.JSIdentifier "set") (ss $1) }
             | 'switch'     { AST.NS (AST.JSIdentifier "switch") (ss $1) }
             | 'this'       { AST.NS (AST.JSIdentifier "this") (ss $1) }
             | 'throw'      { AST.NS (AST.JSIdentifier "throw") (ss $1) }
             | 'true'       { AST.NS (AST.JSIdentifier "true") (ss $1) }
             | 'try'        { AST.NS (AST.JSIdentifier "try") (ss $1) }
             | 'typeof'     { AST.NS (AST.JSIdentifier "typeof") (ss $1) }
             | 'var'        { AST.NS (AST.JSIdentifier "var") (ss $1) }
             | 'void'       { AST.NS (AST.JSIdentifier "void") (ss $1) }
             | 'while'      { AST.NS (AST.JSIdentifier "while") (ss $1) }
             | 'with'       { AST.NS (AST.JSIdentifier "with") (ss $1) }
             | 'future'     { AST.NS (AST.JSIdentifier (token_literal $1)) (ss $1) }  



-- ArrayLiteral :                                                        See 11.1.4
--        [ Elisionopt ]
--        [ ElementList ]
--        [ ElementList , Elisionopt ]
ArrayLiteral :: { AST.JSNode }
ArrayLiteral : '[' ']'                         { AST.NS (AST.JSArrayLiteral []) (ss $1) }
             | '[' Elision ']'                 { AST.NS (AST.JSArrayLiteral $2) (ss $1) }
             | '[' ElementList ']'             { AST.NS (AST.JSArrayLiteral $2) (ss $1) }
             | '[' ElementList ',' Elision ']' { AST.NS (AST.JSArrayLiteral ($2++$4)) (ss $1) }
             | '[' ElementList ',' ']'         { AST.NS (AST.JSArrayLiteral ($2++[AST.NS (AST.JSLiteral ",") (ss $3)])) (ss $1) }

-- ElementList :                                                         See 11.1.4
--        Elisionopt AssignmentExpression
--        ElementList , Elisionopt AssignmentExpression
ElementList :: { [AST.JSNode] }
ElementList : Elision AssignmentExpression                 { (($1)++($2)) {- ElementList -}}
            | AssignmentExpression                         { $1 {- ElementList -}}
            | ElementList ',' Elision AssignmentExpression { (($1)++[(AST.NS (AST.JSElision []) (ss $2))]++($3)++($4)) {- ElementList -}}
            | ElementList ',' AssignmentExpression         { (($1)++[(AST.NS (AST.JSElision []) (ss $2))]++($3)) {- ElementList -}}

-- Elision :                                                             See 11.1.4
--        ,
--        Elision ,
Elision :: { [AST.JSNode] }
Elision :  ','        { [AST.NS (AST.JSElision []) SpanEmpty] }
        | Elision ',' { ($1 ++ [(AST.NS (AST.JSElision []) SpanEmpty)]) }

-- ObjectLiteral :                                                       See 11.1.5
--        { }
--        { PropertyNameAndValueList }
--        { PropertyNameAndValueList , }
ObjectLiteral :: { AST.JSNode }
ObjectLiteral : '{' '}'                          { AST.NS (AST.JSObjectLiteral []) (ss $1)}
              | '{' PropertyNameandValueList '}' { AST.NS (AST.JSObjectLiteral $2) (ss $1)} 
              | '{' PropertyNameandValueList ',' '}' { AST.NS (AST.JSObjectLiteral ($2++[AST.NS (AST.JSLiteral ",") (ss $3)])) (ss $1)} 

-- <Property Name and Value List> ::= <Property Name> ':' <Assignment Expression>
--                                  | <Property Name and Value List> ',' <Property Name> ':' <Assignment Expression>

-- Seems we can have function declarations in the value part too                           
-- PropertyNameAndValueList :                                            See 11.1.5
--        PropertyAssignment
--        PropertyNameAndValueList , PropertyAssignment
PropertyNameandValueList :: { [ AST.JSNode ] }
PropertyNameandValueList : PropertyAssignment                              { [$1] {- PropertyNameandValueList1 -} }
                         | PropertyNameandValueList ',' PropertyAssignment { ($1 ++ [$3]) {- PropertyNameandValueList2 -} }

-- PropertyAssignment :                                                  See 11.1.5
--        PropertyName : AssignmentExpression
--        get PropertyName() { FunctionBody }
--        set PropertyName( PropertySetParameterList ) { FunctionBody }
-- TODO: not clear if get/set are keywords, or just used in a specific context. Puzzling.
PropertyAssignment :: { AST.JSNode }
PropertyAssignment : PropertyName ':' AssignmentExpression { (AST.NS (AST.JSPropertyNameandValue $1 $3) (ex $1)) }
                   -- Should be "get" in next, but is not a Token
                   | 'get' PropertyName '(' ')' '{' FunctionBody '}' { (AST.NS (AST.JSPropertyAccessor "get" $2 [] $6) (ss $1)) }
                   -- Should be "set" in next, but is not a Token
                   | 'set' PropertyName '(' PropertySetParameterList ')' '{' FunctionBody '}' 
                       { (AST.NS (AST.JSPropertyAccessor "set" $2 [$4] $7) (ss $1)) }

-- PropertyName :                                                        See 11.1.5
--        IdentifierName
--        StringLiteral
--        NumericLiteral
PropertyName :: { AST.JSNode }
PropertyName : IdentifierName { $1 {- PropertyName1 -}}
             | StringLiteral  { $1 {- PropertyName2 -}}
             | NumericLiteral { $1 {- PropertyName3 -}}

-- PropertySetParameterList :                                            See 11.1.5
--        Identifier
PropertySetParameterList :: { AST.JSNode }
PropertySetParameterList : Identifier { $1 {- PropertySetParameterList -}}

-- MemberExpression :                                           See 11.2
--        PrimaryExpression
--        FunctionExpression
--        MemberExpression [ Expression ]
--        MemberExpression . IdentifierName
--        new MemberExpression Arguments
MemberExpression :: { [AST.JSNode] }
MemberExpression : PrimaryExpression   { [$1] {- MemberExpression -}} 
                 | FunctionExpression  { [$1] {- MemberExpression -}}
                 | MemberExpression '[' Expression ']' { [AST.NS (AST.JSMemberSquare $1 $3) (mex $1)] }
                 | MemberExpression '.' IdentifierName { [AST.NS (AST.JSMemberDot $1 $3) (mex $1)] } 
                 | 'new' MemberExpression Arguments    { (((AST.NS (AST.JSLiteral "new ") (ss $1)):$2)++[$3])}

-- NewExpression :                                              See 11.2
--        MemberExpression
--        new NewExpression
NewExpression :: { [AST.JSNode] }
NewExpression : MemberExpression {$1 {- NewExpression -}} 
              | 'new' NewExpression { (AST.NS (AST.JSLiteral "new ") (ss $1)):$2 }

-- CallExpression :                                             See 11.2
--        MemberExpression Arguments
--        CallExpression Arguments
--        CallExpression [ Expression ]
--        CallExpression . IdentifierName
CallExpression :: { [AST.JSNode] }
CallExpression : MemberExpression Arguments        { $1++[$2] {- CallExpression -} } 
               | CallExpression Arguments          { ($1++[(AST.NS (AST.JSCallExpression "()" [$2]) (mex $1))]) }
               | CallExpression '[' Expression ']' { ($1++[(AST.NS (AST.JSCallExpression "[]" [$3]) (mex $1))]) }
               | CallExpression '.' IdentifierName { ($1++[(AST.NS (AST.JSCallExpression "."  [$3]) (mex $1))]) }

-- Arguments :                                                  See 11.2
--        ()
--        ( ArgumentList )
Arguments :: { AST.JSNode }
Arguments : '(' ')'               { (AST.NS (AST.JSArguments []) (ss $1)) } 
          | '(' ArgumentList ')'  { (AST.NS (AST.JSArguments $2) (ss $1)) }

-- ArgumentList :                                               See 11.2
--        AssignmentExpression
--        ArgumentList , AssignmentExpression
ArgumentList :: { [[AST.JSNode]] }
ArgumentList : AssignmentExpression { [$1] {- ArgumentList -}}
             | ArgumentList ',' AssignmentExpression { $1++[$3] {- ArgumentList2 -} }

-- LeftHandSideExpression :                                     See 11.2
--        NewExpression
--        CallExpression
LeftHandSideExpression :: { [AST.JSNode] }
LeftHandSideExpression : NewExpression  { $1 {- LeftHandSideExpression1 -}}
                       | CallExpression { $1 {- LeftHandSideExpression12 -}} 

-- PostfixExpression :                                          See 11.3
--        LeftHandSideExpression
--                                  [no LineTerminator here]
--        LeftHandSideExpression                             ++
--                                  [no LineTerminator here]
--        LeftHandSideExpression                             --
PostfixExpression :: { [AST.JSNode] }
PostfixExpression : LeftHandSideExpression { $1 {- PostfixExpression -} } 
                  | PostfixExpression '++' {[(AST.NS (AST.JSExpressionPostfix "++" $1) (mex $1))]}
                  | PostfixExpression '--' {[(AST.NS (AST.JSExpressionPostfix "--" $1) (mex $1))]}

-- UnaryExpression :                                            See 11.4
--        PostfixExpression
--        delete UnaryExpression
--        void UnaryExpression
--        typeof UnaryExpression
--        ++ UnaryExpression
--        -- UnaryExpression
--        + UnaryExpression
--        - UnaryExpression
--        ~ UnaryExpression
--        ! UnaryExpression
UnaryExpression :: { [AST.JSNode] }
UnaryExpression : PostfixExpression { $1 {- UnaryExpression -} } 
                | 'delete' UnaryExpression { ((AST.NS (AST.JSUnary "delete ") (ss $1)):$2)}
                | 'void'   UnaryExpression { ((AST.NS (AST.JSUnary "void ") (ss $1)):$2)}
                | 'typeof' UnaryExpression { ((AST.NS (AST.JSUnary "typeof ") (ss $1)):$2)}
                | '++'     UnaryExpression { ((AST.NS (AST.JSUnary "++") (ss $1)):$2) } 
                | '--'     UnaryExpression { ((AST.NS (AST.JSUnary "--") (ss $1)):$2)}
                | '+'      UnaryExpression { ((AST.NS (AST.JSUnary "+") (ss $1)):$2)}
                | '-'      UnaryExpression { ((AST.NS (AST.JSUnary "-") (ss $1)):$2)}
                | '~'      UnaryExpression { ((AST.NS (AST.JSUnary "~") (ss $1)):$2)}
                | '!'      UnaryExpression { ((AST.NS (AST.JSUnary "!") (ss $1)):$2)}

-- MultiplicativeExpression :                                   See 11.5
--        UnaryExpression
--        MultiplicativeExpression * UnaryExpression
--        MultiplicativeExpression / UnaryExpression
--        MultiplicativeExpression % UnaryExpression
MultiplicativeExpression :: { [AST.JSNode] }
MultiplicativeExpression : UnaryExpression { $1 {- MultiplicativeExpression -}} 
                         | MultiplicativeExpression '*' UnaryExpression { [(AST.NS (AST.JSExpressionBinary "*" $1 $3) (mex $1))]}
                         | MultiplicativeExpression '/' UnaryExpression { [(AST.NS (AST.JSExpressionBinary "/" $1 $3) (mex $1))]}
                         | MultiplicativeExpression '%' UnaryExpression { [(AST.NS (AST.JSExpressionBinary "%" $1 $3) (mex $1))]}

-- AdditiveExpression :                                        See 11.6
--        MultiplicativeExpression
--        AdditiveExpression + MultiplicativeExpression
--        AdditiveExpression - MultiplicativeExpression
AdditiveExpression :: { [AST.JSNode] }
AdditiveExpression : AdditiveExpression '+' MultiplicativeExpression { [(AST.NS (AST.JSExpressionBinary "+" $1 $3) (mex $1))]}
                   | AdditiveExpression '-' MultiplicativeExpression { [(AST.NS (AST.JSExpressionBinary "-" $1 $3) (mex $1))]}
                   | MultiplicativeExpression { $1 {- (goRegExp $1)-} {- AdditiveExpression -} } 

-- ShiftExpression :                                           See 11.7
--        AdditiveExpression
--        ShiftExpression << AdditiveExpression
--        ShiftExpression >> AdditiveExpression
--        ShiftExpression >>> AdditiveExpression
ShiftExpression :: { [AST.JSNode] }
ShiftExpression : ShiftExpression '<<'  AdditiveExpression { [(AST.NS (AST.JSExpressionBinary "<<" $1 $3) (mex $1))]}
                | ShiftExpression '>>'  AdditiveExpression { [(AST.NS (AST.JSExpressionBinary ">>" $1 $3) (mex $1))]}
                | ShiftExpression '>>>' AdditiveExpression { [(AST.NS (AST.JSExpressionBinary ">>>" $1 $3) (mex $1))]}
                | AdditiveExpression { $1 {- ShiftExpression -}} 

-- RelationalExpression :                                      See 11.8
--        ShiftExpression
--        RelationalExpression < ShiftExpression
--        RelationalExpression > ShiftExpression
--        RelationalExpression <= ShiftExpression
--        RelationalExpression >= ShiftExpression
--        RelationalExpression instanceof ShiftExpression
--        RelationalExpression in ShiftExpression
RelationalExpression :: { [AST.JSNode] }
RelationalExpression : ShiftExpression { $1 {- RelationalExpression -}} 
                     | RelationalExpression '<'  ShiftExpression { [(AST.NS (AST.JSExpressionBinary "<" $1 $3) (mex $1))]}
                     | RelationalExpression '>'  ShiftExpression { [(AST.NS (AST.JSExpressionBinary ">" $1 $3) (mex $1))]}
                     | RelationalExpression '<=' ShiftExpression { [(AST.NS (AST.JSExpressionBinary "<=" $1 $3) (mex $1))]}
                     | RelationalExpression '>=' ShiftExpression { [(AST.NS (AST.JSExpressionBinary ">=" $1 $3) (mex $1))]}
                     | RelationalExpression 'instanceof' ShiftExpression { [(AST.NS (AST.JSExpressionBinary " instanceof " $1 $3) (mex $1))]}
                     | RelationalExpression 'in' ShiftExpression { [(AST.NS (AST.JSExpressionBinary " in " $1 $3) (mex $1))]}

-- RelationalExpressionNoIn :                                  See 11.8
--        ShiftExpression
--        RelationalExpressionNoIn < ShiftExpression
--        RelationalExpressionNoIn > ShiftExpression
--        RelationalExpressionNoIn <= ShiftExpression
--        RelationalExpressionNoIn >= ShiftExpression
--        RelationalExpressionNoIn instanceof ShiftExpression
RelationalExpressionNoIn :: { [AST.JSNode] }
RelationalExpressionNoIn : ShiftExpression { $1 {- RelationalExpressionNoIn -}} 
                     | RelationalExpressionNoIn '<'  ShiftExpression { [(AST.NS (AST.JSExpressionBinary "<" $1 $3) (mex $1))]}
                     | RelationalExpressionNoIn '>'  ShiftExpression { [(AST.NS (AST.JSExpressionBinary ">" $1 $3) (mex $1))]}
                     | RelationalExpressionNoIn '<=' ShiftExpression { [(AST.NS (AST.JSExpressionBinary "<=" $1 $3) (mex $1))]}
                     | RelationalExpressionNoIn '>=' ShiftExpression { [(AST.NS (AST.JSExpressionBinary ">=" $1 $3) (mex $1))]}
                     | RelationalExpressionNoIn 'instanceof' ShiftExpression { [(AST.NS (AST.JSExpressionBinary " instanceof " $1 $3) (mex $1))]}

-- EqualityExpression :                                        See 11.9
--        RelationalExpression
--        EqualityExpression == RelationalExpression
--        EqualityExpression != RelationalExpression
--        EqualityExpression === RelationalExpression
--        EqualityExpression !== RelationalExpression
EqualityExpression :: { [AST.JSNode] }
EqualityExpression : RelationalExpression { $1 {- EqualityExpression -} } 
                   | EqualityExpression '=='  RelationalExpression { [(AST.NS (AST.JSExpressionBinary "==" $1 $3) (mex $1))]}
                   | EqualityExpression '!='  RelationalExpression { [(AST.NS (AST.JSExpressionBinary "!=" $1 $3) (mex $1))]}
                   | EqualityExpression '===' RelationalExpression { [(AST.NS (AST.JSExpressionBinary "===" $1 $3) (mex $1))]}
                   | EqualityExpression '!==' RelationalExpression { [(AST.NS (AST.JSExpressionBinary "!==" $1 $3) (mex $1))]}

-- EqualityExpressionNoIn :                                    See 11.9
--        RelationalExpressionNoIn
--        EqualityExpressionNoIn == RelationalExpressionNoIn
--        EqualityExpressionNoIn != RelationalExpressionNoIn
--        EqualityExpressionNoIn === RelationalExpressionNoIn
--        EqualityExpressionNoIn !== RelationalExpressionNoIn
EqualityExpressionNoIn :: { [AST.JSNode] }
EqualityExpressionNoIn : RelationalExpressionNoIn { $1 {- EqualityExpressionNoIn -} } 
                       | EqualityExpressionNoIn '=='  RelationalExpression { [(AST.NS (AST.JSExpressionBinary "==" $1 $3) (mex $1))]}
                       | EqualityExpressionNoIn '!='  RelationalExpression { [(AST.NS (AST.JSExpressionBinary "!=" $1 $3) (mex $1))]}
                       | EqualityExpressionNoIn '===' RelationalExpression { [(AST.NS (AST.JSExpressionBinary "===" $1 $3) (mex $1))]}
                       | EqualityExpressionNoIn '!==' RelationalExpression { [(AST.NS (AST.JSExpressionBinary "!==" $1 $3) (mex $1))]}

-- BitwiseANDExpression :                                      See 11.10
--        EqualityExpression
--        BitwiseANDExpression & EqualityExpression
BitwiseAndExpression :: { [AST.JSNode] }
BitwiseAndExpression : EqualityExpression { $1 {- BitwiseAndExpression -} } 
                     | BitwiseAndExpression '&' EqualityExpression { [(AST.NS (AST.JSExpressionBinary "&" $1 $3) (mex $1))]}

-- BitwiseANDExpressionNoIn :                                  See 11.10
--        EqualityExpressionNoIn
--        BitwiseANDExpressionNoIn & EqualityExpressionNoIn
BitwiseAndExpressionNoIn :: { [AST.JSNode] }
BitwiseAndExpressionNoIn : EqualityExpressionNoIn { $1 {- BitwiseAndExpression -} } 
                     | BitwiseAndExpressionNoIn '&' EqualityExpressionNoIn { [(AST.NS (AST.JSExpressionBinary "&" $1 $3) (mex $1))]}

-- BitwiseXORExpression :                                                                See 11.10
--        BitwiseANDExpression
--        BitwiseXORExpression ^ BitwiseANDExpression
BitwiseXOrExpression :: { [AST.JSNode] }
BitwiseXOrExpression : BitwiseAndExpression { $1 {- BitwiseXOrExpression -} } 
                     | BitwiseXOrExpression '^' BitwiseAndExpression { [(AST.NS (AST.JSExpressionBinary "^" $1 $3) (mex $1))]}

-- BitwiseXORExpressionNoIn :                                                            See 11.10
--        BitwiseANDExpressionNoIn
--        BitwiseXORExpressionNoIn ^ BitwiseANDExpressionNoIn
BitwiseXOrExpressionNoIn :: { [AST.JSNode] }
BitwiseXOrExpressionNoIn : BitwiseAndExpressionNoIn { $1 {- BitwiseXOrExpression -} } 
                         | BitwiseXOrExpressionNoIn '^' BitwiseAndExpressionNoIn { [(AST.NS (AST.JSExpressionBinary "^" $1 $3) (mex $1))]}

-- BitwiseORExpression :                                                                 See 11.10
--        BitwiseXORExpression
--        BitwiseORExpression | BitwiseXORExpression
BitwiseOrExpression :: { [AST.JSNode] }
BitwiseOrExpression : BitwiseXOrExpression { $1 {- BitwiseOrExpression -} } 
                    | BitwiseOrExpression '|' BitwiseXOrExpression { [(AST.NS (AST.JSExpressionBinary "|" $1 $3) (mex $1))]}

-- BitwiseORExpressionNoIn :                                                             See 11.10
--        BitwiseXORExpressionNoIn
--        BitwiseORExpressionNoIn | BitwiseXORExpressionNoIn
BitwiseOrExpressionNoIn :: { [AST.JSNode] }
BitwiseOrExpressionNoIn : BitwiseXOrExpressionNoIn { $1 {- BitwiseOrExpression -} } 
                        | BitwiseOrExpressionNoIn '|' BitwiseXOrExpressionNoIn { [(AST.NS (AST.JSExpressionBinary "|" $1 $3) (mex $1))]}

-- LogicalANDExpression :                                                                See 11.11
--        BitwiseORExpression
--        LogicalANDExpression && BitwiseORExpression
LogicalAndExpression :: { [AST.JSNode] }
LogicalAndExpression : BitwiseOrExpression { $1 {- LogicalAndExpression -} } 
                     | LogicalAndExpression '&&' BitwiseOrExpression { [(AST.NS (AST.JSExpressionBinary "&&" $1 $3) (mex $1))]}

-- LogicalANDExpressionNoIn :                                                            See 11.11
--        BitwiseORExpressionNoIn
--        LogicalANDExpressionNoIn && BitwiseORExpressionNoIn
LogicalAndExpressionNoIn :: { [AST.JSNode] }
LogicalAndExpressionNoIn : BitwiseOrExpressionNoIn { $1 {- LogicalAndExpression -} } 
                         | LogicalAndExpressionNoIn '&&' BitwiseOrExpressionNoIn { [(AST.NS (AST.JSExpressionBinary "&&" $1 $3) (mex $1))]}

-- LogicalORExpression :                                                                 See 11.11
--        LogicalANDExpression
--        LogicalORExpression || LogicalANDExpression
LogicalOrExpression :: { [AST.JSNode] }
LogicalOrExpression : LogicalAndExpression { $1 {- LogicalOrExpression -} } 
                    | LogicalOrExpression '||' LogicalAndExpression { [(AST.NS (AST.JSExpressionBinary "||" $1 $3) (mex $1))]}

-- LogicalORExpressionNoIn :                                                             See 11.11
--        LogicalANDExpressionNoIn
--        LogicalORExpressionNoIn || LogicalANDExpressionNoIn
LogicalOrExpressionNoIn :: { [AST.JSNode] }
LogicalOrExpressionNoIn : LogicalAndExpressionNoIn { $1 {- LogicalOrExpression -} } 
                        | LogicalOrExpressionNoIn '||' LogicalAndExpressionNoIn { [(AST.NS (AST.JSExpressionBinary "||" $1 $3) (mex $1))]}

-- ConditionalExpression :                                                               See 11.12
--        LogicalORExpression
--        LogicalORExpression ? AssignmentExpression : AssignmentExpression
ConditionalExpression :: { [AST.JSNode] }
ConditionalExpression : LogicalOrExpression { $1 {- ConditionalExpression -} }
                      | LogicalOrExpression '?' AssignmentExpression ':' AssignmentExpression 
                        { [AST.NS (AST.JSExpressionTernary $1 $3 $5) (mex $1)] } 
                    
-- ConditionalExpressionNoIn :                                                           See 11.12
--        LogicalORExpressionNoIn
--        LogicalORExpressionNoIn ? AssignmentExpressionNoIn : AssignmentExpressionNoIn
ConditionalExpressionNoIn :: { [AST.JSNode] }
ConditionalExpressionNoIn : LogicalOrExpressionNoIn { $1 {- ConditionalExpression -} }
                          | LogicalOrExpressionNoIn '?' AssignmentExpressionNoIn ':' AssignmentExpressionNoIn
                            { [AST.NS (AST.JSExpressionTernary $1 $3 $5) (mex $1)] } 
  
-- AssignmentExpression :                                                                See 11.13
--        ConditionalExpression
--        LeftHandSideExpression AssignmentOperator AssignmentExpression
AssignmentExpression :: { [AST.JSNode] }
AssignmentExpression : ConditionalExpression { $1 {- AssignmentExpression -}} 
                     | LeftHandSideExpression AssignmentOperator AssignmentExpression 
                       { ($1++[$2]++$3) }
                       
-- AssignmentExpressionNoIn :                                                            See 11.13
--        ConditionalExpressionNoIn
--        LeftHandSideExpression AssignmentOperator AssignmentExpressionNoIn
AssignmentExpressionNoIn :: { [AST.JSNode] }
AssignmentExpressionNoIn : ConditionalExpressionNoIn { $1 {- AssignmentExpression -}} 
                         | LeftHandSideExpression AssignmentOperator AssignmentExpressionNoIn 
                           { ($1++[$2]++$3) }

-- AssignmentOperator : one of                                                           See 11.13
--     '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '>>>=' | '&=' | '^=' | '|='
AssignmentOperator :: { AST.JSNode }
AssignmentOperator : 'assign' { AST.NS (AST.JSOperator (token_literal $1)) (ss $1) }
                   | '='      { AST.NS (AST.JSOperator "=") (ss $1)}

-- Expression :                                                   See 11.14
--         AssignmentExpression
--         Expression , AssignmentExpression
Expression :: { AST.JSNode }
Expression : AssignmentExpression { AST.NS (AST.JSExpression $1) (mex $1) {- Expression -} } 
           | Expression ',' AssignmentExpression  { flattenExpression $1 $3 }

-- ExpressionNoIn :                                               See 11.14
--         AssignmentExpressionNoIn
--         ExpressionNoIn , AssignmentExpressionNoIn
ExpressionNoIn :: { AST.JSNode }
ExpressionNoIn : AssignmentExpressionNoIn { AST.NS (AST.JSExpression $1) (mex $1) {- Expression -} } 
               | ExpressionNoIn ',' AssignmentExpressionNoIn  { flattenExpression $1 $3 }

-- TODO: still required?
ExpressionOpt :: { [AST.JSNode] }
ExpressionOpt : Expression { [$1] {- ExpressionOpt -}}
              |            { []   {- ExpressionOpt -}}

ExpressionNoInOpt :: { [AST.JSNode] }
ExpressionNoInOpt : ExpressionNoIn { [$1] {- ExpressionOpt -}}
                  |            { []   {- ExpressionOpt -}}
                               

-- Statement :                                                    See clause 12
--         Block
--         VariableStatement
--         EmptyStatement
--         ExpressionStatement
--         IfStatement
--         IterationStatement
--         ContinueStatement
--         BreakStatement
--         ReturnStatement
--         WithStatement
--         LabelledStatement
--         SwitchStatement
--         ThrowStatement
--         TryStatement
--         DebuggerStatement
Statement :: { AST.JSNode }
Statement : StatementNoEmpty   { $1 {- Statement1 -}}
          | EmptyStatement     { $1 {- Statement3 -}}

StatementNoEmpty :: { AST.JSNode }
StatementNoEmpty : StatementBlock     { $1 {- StatementNoEmpty1 -}}
                 | VariableStatement     { $1 {- StatementNoEmpty2 -}}
                   -- | EmptyStatement     { $1 {- StatementNoEmpty3 -}}
                 | ExpressionStatement  { $1 {- StatementNoEmpty4 -}}
                 | IfStatement        { $1 {- StatementNoEmpty5 -}}
                 | IterationStatement { $1 {- StatementNoEmpty6 -}}
                 | ContinueStatement  { $1 {- StatementNoEmpty7 -}}
                 | BreakStatement     { $1 {- StatementNoEmpty8 -}}
                 | ReturnStatement    { $1 {- StatementNoEmpty9 -}}
                 | WithStatement      { $1 {- StatementNoEmpty10 -}}
                 | LabelledStatement  { $1 {- StatementNoEmpty11 -}}
                 | SwitchStatement    { $1 {- StatementNoEmpty12 -}}
                 | ThrowStatement     { $1 {- StatementNoEmpty13 -}}
                 | TryStatement       { $1 {- StatementNoEmpty14 -}}
                 | DebuggerStatement  { $1 {- StatementNoEmpty15 -}}

-- FIXME: not producing true reflection of parse, doing minimisation transformationsa  in place
-- StatementBlock :: { AST.JSNode }
-- StatementBlock : '{' '}'               { (AST.NS (AST.JSLiteral ";") (ss $1)) }
--                | '{' StatementList '}' { (if ($2 == AST.JSStatementList [AST.JSLiteral ";"]) 
--                                             then (AST.NS (AST.JSLiteral ";") (ss $1)) 
--                                             else (AST.NS (AST.JSBlock $2   ) (ss $1))) 
--                                        }
StatementBlock :: { AST.JSNode }
StatementBlock : '{' '}'               { (AST.NS (AST.JSStatementBlock (AST.NS (AST.JSStatementList []) (ss $1))) (ss $1)) }
               | '{' StatementList '}' { (AST.NS (AST.JSStatementBlock $2) (ss $1)) }

-- Block :                                                        See 12.1
--         { StatementListopt }
Block :: { AST.JSNode }
Block : '{' '}'               { (AST.NS (AST.JSBlock (AST.NS (AST.JSStatementList []) (ss $1))) (ss $1)) }
      | '{' StatementList '}' { (AST.NS (AST.JSBlock $2) (ss $1)) }

-- StatementList :                                                See 12.1
--         Statement
--         StatementList Statement
StatementList :: { AST.JSNode }
StatementList : Statement               { (AST.NS (AST.JSStatementList [$1]) (ex $1)) }
              | StatementList Statement { (combineStatements $1 $2) }

-- VariableStatement :                                            See 12.2
--         var VariableDeclarationList ;
VariableStatement :: { AST.JSNode }
VariableStatement : 'var'   VariableDeclarationList AutoSemi { AST.NS (AST.JSVariables "var" $2) (ss $1)}
                  | 'const' VariableDeclarationList AutoSemi { AST.NS (AST.JSVariables "const" $2) (ss $1)}

-- VariableDeclarationList :                                      See 12.2
--         VariableDeclaration
--         VariableDeclarationList , VariableDeclaration
VariableDeclarationList :: { [AST.JSNode] }
VariableDeclarationList : VariableDeclaration { [$1] {- VariableDeclarationList -}}
                        | VariableDeclarationList ',' VariableDeclaration { ($1 ++ [$3]) {- VariableDeclarationList -}}

-- VariableDeclarationListNoIn :                                  See 12.2
--         VariableDeclarationNoIn
--         VariableDeclarationListNoIn , VariableDeclarationNoIn
VariableDeclarationListNoIn :: { [AST.JSNode] }
VariableDeclarationListNoIn : VariableDeclarationNoIn { [$1] {- VariableDeclarationList -}}
                            | VariableDeclarationListNoIn ',' VariableDeclarationNoIn { ($1 ++ [$3]) {- VariableDeclarationList -}}

-- VariableDeclaration :                                          See 12.2
--         Identifier Initialiseropt
VariableDeclaration :: { AST.JSNode }
VariableDeclaration : Identifier              { (AST.NS (AST.JSVarDecl $1 []) (ex $1) )}
                    | Identifier Initializer  { (AST.NS (AST.JSVarDecl $1 $2) (ex $1))}

-- VariableDeclarationNoIn :                                      See 12.2
--         Identifier InitialiserNoInopt
VariableDeclarationNoIn :: { AST.JSNode }
VariableDeclarationNoIn : Identifier InitializerNoIn { (AST.NS (AST.JSVarDecl $1 $2) (ex $1)) }
                        | Identifier                 { (AST.NS (AST.JSVarDecl $1 []) (ex $1)) }

-- Initialiser :                                                                            See 12.2
--         = AssignmentExpression
Initializer :: { [AST.JSNode] }
Initializer : '=' AssignmentExpression { $2 {- Initializer -} }

-- InitialiserNoIn :                                                                        See 12.2
--         = AssignmentExpressionNoIn
InitializerNoIn :: { [AST.JSNode] }
InitializerNoIn : '=' AssignmentExpressionNoIn { $2 {- InitializerNoIn -}}

-- EmptyStatement :                                                                         See 12.3
--         ;
EmptyStatement :: { AST.JSNode }
EmptyStatement : ';' { (AST.NS (AST.JSLiteral ";") (ss $1)) }

-- ExpressionStatement :                                                                    See 12.4
--         [lookahead ∉ {{, function}] Expression  ;
-- TODO: Sort out lookahead issue. Maybe by just putting production lower to set reduce/reduce conflict
--       According to http://sideshowbarker.github.com/es5-spec/#x12.4, the ambiguity is with
--       Block or FunctionDeclaration
ExpressionStatement :: { AST.JSNode }
ExpressionStatement : Expression { $1 {- ExpressionStatement -} }


-- IfStatement :                                                                            See 12.5
--         if ( Expression ) Statement else Statement
--         if ( Expression ) Statement
IfStatement :: { AST.JSNode }
IfStatement : 'if' '(' Expression ')' StatementSemi  IfElseRest 
                  { (if ($6 /= []) then  
                       (if (length $6 == 1) 
                        then 
                          (AST.NS (AST.JSIfElse $3 $5 (head $6)) (ss $1)) 
                        else 
                          (AST.NS 
                             (AST.JSIfElse 
                                $3 
                                (AST.NS 
                                   (AST.JSBlock (AST.NS (AST.JSStatementList [$5]) (ss $1)) ) 
                                   (ss $1) 
                                   )
                                (last $6)
                             ) 
                             (ss $1)
                           ) 
                       )   
                     else (AST.NS (AST.JSIf $3 $5) (ss $1))) }
                  
IfElseRest :: { [AST.JSNode] }                  
IfElseRest : 'else' Statement     { [$2] }
           |                      { [] } 

StatementSemi :: { AST.JSNode }
StatementSemi : StatementNoEmpty ';' { (AST.NS (AST.JSBlock (AST.NS (AST.JSStatementList [$1]) (ex $1))) (ex $1)) } 
              | StatementNoEmpty     { $1 {- StatementSemi -}}
              | ';'                  { AST.NS (AST.JSLiteral ";") (ss $1)  }  


-- IterationStatement :                                                                     See 12.6
--         do Statement while ( Expression );
--         while ( Expression ) Statement
--         for (ExpressionNoInopt; Expressionopt ; Expressionopt ) Statement
--         for ( var VariableDeclarationListNoIn; Expressionopt ; Expressionopt ) Statement
--         for ( LeftHandSideExpression in Expression ) Statement
--         for ( var VariableDeclarationNoIn in Expression ) Statement
IterationStatement :: { AST.JSNode }
IterationStatement : 'do' Statement 'while' '(' Expression ')' AutoSemi 
                     { (AST.NS (AST.JSDoWhile $2 $5 $7) (ss $1)) } 
                   | 'while' '(' Expression ')' Statement 
                     { (AST.NS (AST.JSWhile $3 $5) (ss $1)) }
                   | 'for' '(' ExpressionNoInOpt ';' ExpressionOpt ';' ExpressionOpt ')' Statement 
                     { (AST.NS (AST.JSFor $3 $5 $7 $9) (ss $1)) }
                   | 'for' '(' 'var' VariableDeclarationListNoIn ';' ExpressionOpt ';' ExpressionOpt ')' Statement 
                     { (AST.NS (AST.JSForVar $4 $6 $8 $10) (ss $1)) }
                   | 'for' '(' LeftHandSideExpression 'in' Expression ')' Statement 
                     { (AST.NS (AST.JSForIn $3 $5 $7) (ss $1)) }
                   | 'for' '(' 'var' VariableDeclarationNoIn 'in' Expression ')' Statement
                     { (AST.NS (AST.JSForVarIn $4 $6 $8) (ss $1)) }

-- ContinueStatement :                                                                      See 12.7
--         continue [no LineTerminator here] Identifieropt ;
-- TODO: deal with [no LineTerminator here]
ContinueStatement :: { AST.JSNode }
ContinueStatement : 'continue' AutoSemi             { (AST.NS (AST.JSContinue [$2]) (ss $1)) } 
                  | 'continue' Identifier AutoSemi  { (AST.NS (AST.JSContinue [$2,$3]) (ss $1)) } 

-- BreakStatement :                                                                         See 12.8
--         break [no LineTerminator here] Identifieropt ;
-- TODO: deal with [no LineTerminator here]
BreakStatement :: { AST.JSNode }
BreakStatement : 'break' AutoSemi             { (AST.NS (AST.JSBreak []   [$2]) (ss $1)) } 
               | 'break' Identifier AutoSemi  { (AST.NS (AST.JSBreak [$2] [$3]) (ss $1)) } 

-- ReturnStatement :                                                                        See 12.9
--         return [no LineTerminator here] Expressionopt ;
-- TODO: deal with [no LineTerminator here]
ReturnStatement :: { AST.JSNode }
ReturnStatement : 'return' AutoSemi             { (AST.NS (AST.JSReturn [$2])    (ss $1)) } 
                | 'return' Expression AutoSemi  { (AST.NS (AST.JSReturn [$2,$3]) (ss $1)) } 

-- WithStatement :                                                                          See 12.10
--         with ( Expression ) Statement
WithStatement :: { AST.JSNode }
WithStatement : 'with' '(' Expression ')' Statement AutoSemi  { (AST.NS (AST.JSWith $3 [$5,$6]) (ss $1)) }

-- SwitchStatement :                                                                        See 12.11
--         switch ( Expression ) CaseBlock
SwitchStatement :: { AST.JSNode }
SwitchStatement : 'switch' '(' Expression ')' CaseBlock { (AST.NS (AST.JSSwitch $3 $5) (ss $1)) } 

-- CaseBlock :                                                                              See 12.11
--         { CaseClausesopt }
--         { CaseClausesopt DefaultClause CaseClausesopt }
CaseBlock :: { [AST.JSNode] }
CaseBlock : '{' CaseClausesOpt '}'                              { $2            {- CaseBlock1 -}}
          | '{' CaseClausesOpt DefaultClause CaseClausesOpt '}' { ($2++($3:$4)) {- CaseBlock2 -}}

-- CaseClauses :                                                                            See 12.11
--         CaseClause
--         CaseClauses CaseClause
CaseClausesOpt :: { [AST.JSNode] }
CaseClausesOpt : CaseClause                { [$1] {- CaseClauses1 -}}
               | CaseClausesOpt CaseClause { ($1++[$2]) {- CaseClauses2 -}}
               |                           { [] }  

-- CaseClause :                                                               See 12.11
--        case Expression : StatementListopt
CaseClause :: { AST.JSNode }
CaseClause : 'case' Expression ':' StatementList  { (AST.NS (AST.JSCase $2 $4) (ss $1)) }
           | 'case' Expression ':'                { (AST.NS (AST.JSCase $2 (AST.NS (AST.JSStatementList []) (ss $1))) (ss $1)) }

-- DefaultClause :                                                            See 12.11
--        default : StatementListopt
DefaultClause :: { AST.JSNode }
DefaultClause : 'default' ':'                { (AST.NS (AST.JSDefault (AST.NS (AST.JSStatementList []) (ss $1))) (ss $1)) }
              | 'default' ':' StatementList  { (AST.NS (AST.JSDefault $3) (ss $1)) }

-- LabelledStatement :                                                        See 12.12
--        Identifier : Statement
LabelledStatement :: { AST.JSNode }
LabelledStatement : Identifier ':' Statement { (AST.NS (AST.JSLabelled $1 $3) (ex $1)) }

-- ThrowStatement :                                                           See 12.13
--        throw [no LineTerminator here] Expression ;
-- TODO : sort out no LineTerminator here
--        Does it need a semi at the end?
ThrowStatement :: { AST.JSNode }
ThrowStatement : 'throw' Expression { (AST.NS (AST.JSThrow $2) (ss $1)) }

-- Note: worked in updated syntax as per https://developer.mozilla.org/en/JavaScript/Reference/Statements/try...catch
--   i.e., 0 or more catches, then an optional finally
-- TryStatement :                                                             See 12.14
--        try Block Catch
--        try Block Finally
--        try Block Catch Finally
TryStatement :: { AST.JSNode }
TryStatement : 'try' Block Catches         { (AST.NS (AST.JSTry $2 $3) (ss $1))         {- TryStatement1 -} }
             | 'try' Block Finally         { (AST.NS (AST.JSTry $2 [$3]) (ss $1))       {- TryStatement2 -} }
             | 'try' Block Catches Finally { (AST.NS (AST.JSTry $2 ($3++[$4])) (ss $1)) {- TryStatement3 -} }

Catches :: { [AST.JSNode] }
Catches : Catch         { [$1]       {- Catches 1 -} }
        | Catches Catch { ($1++[$2]) {- Catches 2 -} }

-- Note: worked in updated syntax as per https://developer.mozilla.org/en/JavaScript/Reference/Statements/try...catch
-- <Catch> ::= 'catch' '(' Identifier ')' <Block>
--   becomes
-- <Catch> ::= 'catch' '(' Identifier ')' <Block>
--           | 'catch' '(' Identifier 'if' ConditionalExpression ')' <Block>
Catch :: { AST.JSNode }
Catch : 'catch' '(' Identifier ')' Block                 { (AST.NS (AST.JSCatch $3 [] $5) (ss $1)) }
      | 'catch' '(' Identifier 'if' ConditionalExpression ')' Block  { (AST.NS (AST.JSCatch $3 $5 $7) (ss $1)) }

-- Finally :                                                                  See 12.14
--        finally Block
Finally :: { AST.JSNode }
Finally : 'finally' Block { (AST.NS (AST.JSFinally $2) (ss $1)) }

-- DebuggerStatement :                                                        See 12.15
--        debugger ;
DebuggerStatement :: { AST.JSNode }
DebuggerStatement : 'debugger' AutoSemi { (AST.NS (AST.JSLiteral "debugger") (ss $1)) }

-- FunctionDeclaration :                                                      See clause 13
--        function Identifier ( FormalParameterListopt ) { FunctionBody }
FunctionDeclaration :: { AST.JSNode }
FunctionDeclaration : 'function' Identifier '(' FormalParameterList ')' '{' FunctionBody '}'
                      { (AST.NS (AST.JSFunction $2 $4 $7) (ss $1)) }
                    | 'function' Identifier '(' ')' '{' FunctionBody '}'
                      { (AST.NS (AST.JSFunction $2 [] $6) (ss $1)) }

-- FunctionExpression :                                                       See clause 13
--        function Identifieropt ( FormalParameterListopt ) { FunctionBody }
FunctionExpression :: { AST.JSNode }
FunctionExpression : 'function' IdentifierOpt '(' ')' '{' FunctionBody '}'                      
                     { (AST.NS (AST.JSFunctionExpression $2 [] $6) (ss $1)) }
                   | 'function' IdentifierOpt '(' FormalParameterList ')' '{' FunctionBody '}'  
                     { (AST.NS (AST.JSFunctionExpression $2 $4 $7) (ss $1)) }

IdentifierOpt :: { [AST.JSNode] }
IdentifierOpt : Identifier { [$1] {- IdentifierOpt -}}
              |            { []   {- IdentifierOpt -}}

-- FormalParameterList :                                                      See clause 13
--        Identifier
--        FormalParameterList , Identifier
FormalParameterList :: { [AST.JSNode] }
FormalParameterList : Identifier                          { [$1] {- FormalParameterList -}}
                    | FormalParameterList ',' Identifier  { ($1++[$3]) }

-- FunctionBody :                                                             See clause 13
--        SourceElementsopt
FunctionBody :: { AST.JSNode }
FunctionBody : SourceElements { (AST.NS (AST.JSFunctionBody [$1]) (ex $1)) }
             |                { (AST.NS (AST.JSFunctionBody [])   SpanEmpty) } 

-- Program :                                                                  See clause 14
--        SourceElementsopt

Program :: { AST.JSNode }
Program : SourceElementsTop { $1 {- Program -}}

-- SourceElements :                                                           See clause 14
--        SourceElement
--        SourceElements SourceElement
SourceElements :: { AST.JSNode }
SourceElements : SourceElement                { (AST.NS (AST.JSSourceElements [$1]) (ex $1)) }
               | SourceElements SourceElement { (combineSourceElements $1 $2) }

SourceElementsTop :: { AST.JSNode }
SourceElementsTop : SourceElement                   { (AST.NS (AST.JSSourceElementsTop [$1]) (ex $1)) }
                  | SourceElementsTop SourceElement { (combineSourceElementsTop $1 $2) }

-- SourceElement :
--       Statement
--       FunctionDeclaration
SourceElement :: { AST.JSNode }
SourceElement : Statement            { $1 {- SourceElement1 -} }
              | FunctionDeclaration  { $1 {- SourceElement2 -} } 

{

-- combineSourceElements :: AST.JSNode -> AST.JSNode -> AST.JSNode
-- combineSourceElements (AST.JSSourceElements xs) x = (AST.JSSourceElements (xs++[x] (ss $1)) )
combineSourceElements :: AST.JSNode -> AST.JSNode -> AST.JSNode
combineSourceElements (AST.NS (AST.JSSourceElements xs) s1) x1@(AST.NS x s2) = (AST.NS (AST.JSSourceElements (xs++[x1])) s1) 

-- combineSourceElementsTop :: AST.JSNode -> AST.JSNode -> AST.JSNode
-- combineSourceElementsTop (AST.JSSourceElementsTop xs) x = (AST.JSSourceElementsTop (xs++[x] (ss $1)) )
combineSourceElementsTop :: AST.JSNode -> AST.JSNode -> AST.JSNode
combineSourceElementsTop (AST.NS (AST.JSSourceElementsTop xs) s1) x1@(AST.NS x s2) = (AST.NS (AST.JSSourceElementsTop (xs++[x1])) s1)

-- combineStatements :: AST.JSNode -> AST.JSNode -> AST.JSNode
-- combineStatements (AST.JSStatementList xs) (AST.JSStatementList ys) = (AST.JSStatementList (xs++ys) )
-- combineStatements (AST.JSStatementList xs) y = (AST.JSStatementList (xs++[y]) )
combineStatements :: AST.JSNode -> AST.JSNode -> AST.JSNode
combineStatements (AST.NS (AST.JSStatementList xs) s1) (AST.NS (AST.JSStatementList ys) s2) = (AST.NS (AST.JSStatementList (xs++ys) ) s1)
combineStatements (AST.NS (AST.JSStatementList xs) s1) y = (AST.NS (AST.JSStatementList (xs++[y])) s1)

parseError :: Token -> P a 
parseError = throwError . UnexpectedToken 

-- flattenExpression :: AST.JSNode -> [AST.JSNode] -> AST.JSNode
-- flattenExpression (AST.JSExpression xs) e = AST.JSExpression (xs++litComma++e)
--                         where
--                           litComma :: [AST.JSNode]
--                           litComma = [(AST.JSLiteral ",")]
flattenExpression :: AST.JSNode -> [AST.JSNode] -> AST.JSNode
flattenExpression (AST.NS (AST.JSExpression xs) s1) e = (AST.NS (AST.JSExpression (xs++litComma++e)) s1)
                        where
                          litComma :: [AST.JSNode]
                          litComma = [AST.NS (AST.JSLiteral ",") SpanEmpty]

mex :: [AST.JSNode] -> SrcSpan
mex [] = SpanEmpty
mex xs = ex (head xs)

ex :: AST.JSNode -> SrcSpan
ex (AST.NS _node span) = span

ss token = toSrcSpan (token_span token) 

{-
ex :: AST.JSNode -> AST.JSNode
ex (AST.NS node span) = node

mex :: [AST.JSNode] -> [AST.JSNode]
mex xs = map ex xs

combine :: [AST.JSNode] -> AST.JSNode
combine [] = AST.NSS [] SpanEmpty
combine xs = AST.NSS (mex xs) span
  where
    (AST.NS _ span) = head xs
-}
    
}

-- Set emacs mode
-- Local Variables: 
-- mode:haskell
-- End:             
