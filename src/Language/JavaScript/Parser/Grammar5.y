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
AutoSemi : ';' { AST.JSLiteral ";"}
         |     { AST.JSLiteral ""}
           
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
NullLiteral : 'null' { AST.JSLiteral "null" }

BooleanLiteral :: { AST.JSNode }
BooleanLiteral : 'true' { AST.JSLiteral "true" }
               | 'false' { AST.JSLiteral "false" }

-- <Numeric Literal> ::= DecimalLiteral
--                     | HexIntegerLiteral
NumericLiteral :: { AST.JSNode }
NumericLiteral : 'decimal'    { AST.JSDecimal (token_literal $1)}
               | 'hexinteger' { AST.JSHexInteger (token_literal $1)}

StringLiteral :: { AST.JSNode }
StringLiteral : 'string'  {AST.JSStringLiteral (token_delimiter $1) (token_literal $1)}

-- <Regular Expression Literal> ::= RegExp 
RegularExpressionLiteral :: { AST.JSNode }
RegularExpressionLiteral : 'regex' {AST.JSRegEx (token_literal $1)}

-- PrimaryExpression :                                                   See 11.1
--        this
--        Identifier
--        Literal
--        ArrayLiteral
--        ObjectLiteral
--        ( Expression )
PrimaryExpression :: { AST.JSNode }
PrimaryExpression : 'this'                   { AST.JSLiteral "this" }
                  | Identifier               { $1 {- PrimaryExpression1 -}}
                  | Literal                  { $1 {- PrimaryExpression2 -}}
                  | ArrayLiteral             { $1 {- PrimaryExpression3 -}}
                  | ObjectLiteral            { $1 {- PrimaryExpression4 -}}
                  | '(' Expression ')'       { AST.JSExpressionParen $2 }
                  
-- Identifier ::                                                            See 7.6
--         IdentifierName but not ReservedWord
-- IdentifierName ::                                                        See 7.6
--         IdentifierStart
--         IdentifierName IdentifierPart
Identifier :: { AST.JSNode }
Identifier : 'ident' { AST.JSIdentifier (token_literal $1) }
-- TODO: make this include any reserved word too, including future
IdentifierName :: { AST.JSNode }
IdentifierName : Identifier {$1}

-- ArrayLiteral :                                                        See 11.1.4
--        [ Elisionopt ]
--        [ ElementList ]
--        [ ElementList , Elisionopt ]
ArrayLiteral :: { AST.JSNode }
ArrayLiteral : '[' ']'                         { AST.JSArrayLiteral [] }
             | '[' Elision ']'                 { AST.JSArrayLiteral $2 }
             | '[' ElementList ']'             { AST.JSArrayLiteral $2 }
             | '[' ElementList ',' Elision ']' { AST.JSArrayLiteral ($2++$4) }
             | '[' ElementList ',' ']'         { AST.JSArrayLiteral ($2++[AST.JSLiteral ","]) }

-- ElementList :                                                         See 11.1.4
--        Elisionopt AssignmentExpression
--        ElementList , Elisionopt AssignmentExpression
ElementList :: { [AST.JSNode] }
ElementList : Elision AssignmentExpression                 { ($1++$2) {- ElementList -}}
            | AssignmentExpression                         { $1 {- ElementList -}}
            | ElementList ',' Elision AssignmentExpression { ($1++[(AST.JSElision [])]++$3++$4) {- ElementList -}}
            | ElementList ',' AssignmentExpression         { ($1++[(AST.JSElision [])]++$3) {- ElementList -}}

-- Elision :                                                             See 11.1.4
--        ,
--        Elision ,
Elision :: { [AST.JSNode] }
Elision :  ','        { [(AST.JSElision [])] }
        | Elision ',' { $1 ++ [(AST.JSElision [])] }

-- ObjectLiteral :                                                       See 11.1.5
--        { }
--        { PropertyNameAndValueList }
--        { PropertyNameAndValueList , }
ObjectLiteral :: { AST.JSNode }
ObjectLiteral : '{' '}'                          { AST.JSObjectLiteral [] }
              | '{' PropertyNameandValueList '}' { AST.JSObjectLiteral $2 } 
              | '{' PropertyNameandValueList ',' '}' { AST.JSObjectLiteral ($2++[AST.JSLiteral ","]) } 

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
PropertyAssignment : PropertyName ':' AssignmentExpression { (AST.JSPropertyNameandValue $1 $3) }
                   | 'get' PropertyName '(' ')' '{' FunctionBody '}' { (AST.JSPropertyAccessor "get" $2 [] $6) }
                   | 'set' PropertyName '(' PropertySetParameterList ')' '{' FunctionBody '}' 
                       { (AST.JSPropertyAccessor "set" $2 [$4] $7) }

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
                 | MemberExpression '[' Expression ']' { [AST.JSMemberSquare $1 $3] }
                 | MemberExpression '.' IdentifierName { [AST.JSMemberDot $1 $3] } 
                 | 'new' MemberExpression Arguments    { (((AST.JSLiteral "new "):$2)++[$3])}

-- MARK --

-- NewExpression :                                              See 11.2
--        MemberExpression
--        new NewExpression
NewExpression :: { [AST.JSNode] }
NewExpression : MemberExpression {$1 {- NewExpression -}} 
              | 'new' NewExpression { (AST.JSLiteral "new "):$2 }

-- CallExpression :                                             See 11.2
--        MemberExpression Arguments
--        CallExpression Arguments
--        CallExpression [ Expression ]
--        CallExpression . IdentifierName
CallExpression :: { [AST.JSNode] }
CallExpression : MemberExpression Arguments        { $1++[$2] {- CallExpression -} } 
               | CallExpression Arguments          { ($1++[(AST.JSCallExpression "()" [$2])]) }
               | CallExpression '[' Expression ']' { ($1++[(AST.JSCallExpression "[]" [$3])]) }
               | CallExpression '.' IdentifierName { ($1++[(AST.JSCallExpression "."  [$3])]) }

-- Arguments :                                                  See 11.2
--        ()
--        ( ArgumentList )
Arguments :: { AST.JSNode }
Arguments : '(' ')'               { (AST.JSArguments []) } 
          | '(' ArgumentList ')'  { (AST.JSArguments $2) }

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
                  | PostfixExpression '++' {[(AST.JSExpressionPostfix "++" $1)]}
                  | PostfixExpression '--' {[(AST.JSExpressionPostfix "--" $1)]}

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
                | 'delete' UnaryExpression { ((AST.JSUnary "delete "):$2)}
                | 'void'   UnaryExpression { ((AST.JSUnary "void "):$2)}
                | 'typeof' UnaryExpression { ((AST.JSUnary "typeof "):$2)}
                | '++'     UnaryExpression { ((AST.JSUnary "++"):$2) } 
                | '--'     UnaryExpression { ((AST.JSUnary "--"):$2)}
                | '+'      UnaryExpression { ((AST.JSUnary "+"):$2)}
                | '-'      UnaryExpression { ((AST.JSUnary "-"):$2)}
                | '~'      UnaryExpression { ((AST.JSUnary "~"):$2)}
                | '!'      UnaryExpression { ((AST.JSUnary "!"):$2)}

-- MultiplicativeExpression :                                   See 11.5
--        UnaryExpression
--        MultiplicativeExpression * UnaryExpression
--        MultiplicativeExpression / UnaryExpression
--        MultiplicativeExpression % UnaryExpression
MultiplicativeExpression :: { [AST.JSNode] }
MultiplicativeExpression : UnaryExpression { $1 {- MultiplicativeExpression -}} 
                         | MultiplicativeExpression '*' UnaryExpression { [(AST.JSExpressionBinary "*" $1 $3)]}
                         | MultiplicativeExpression '/' UnaryExpression { [(AST.JSExpressionBinary "/" $1 $3)]}
                         | MultiplicativeExpression '%' UnaryExpression { [(AST.JSExpressionBinary "%" $1 $3)]}

-- AdditiveExpression :                                        See 11.6
--        MultiplicativeExpression
--        AdditiveExpression + MultiplicativeExpression
--        AdditiveExpression - MultiplicativeExpression
AdditiveExpression :: { [AST.JSNode] }
AdditiveExpression : AdditiveExpression '+' MultiplicativeExpression { [(AST.JSExpressionBinary "+" $1 $3)]}
                   | AdditiveExpression '-' MultiplicativeExpression { [(AST.JSExpressionBinary "-" $1 $3)]}
                   | MultiplicativeExpression { $1 {- (goRegExp $1)-} {- AdditiveExpression -} } 

-- ShiftExpression :                                           See 11.7
--        AdditiveExpression
--        ShiftExpression << AdditiveExpression
--        ShiftExpression >> AdditiveExpression
--        ShiftExpression >>> AdditiveExpression
ShiftExpression :: { [AST.JSNode] }
ShiftExpression : ShiftExpression '<<'  AdditiveExpression { [(AST.JSExpressionBinary "<<" $1 $3)]}
                | ShiftExpression '>>'  AdditiveExpression { [(AST.JSExpressionBinary ">>" $1 $3)]}
                | ShiftExpression '>>>' AdditiveExpression { [(AST.JSExpressionBinary ">>>" $1 $3)]}
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
                     | RelationalExpression '<'  ShiftExpression { [(AST.JSExpressionBinary "<" $1 $3)]}
                     | RelationalExpression '>'  ShiftExpression { [(AST.JSExpressionBinary ">" $1 $3)]}
                     | RelationalExpression '<=' ShiftExpression { [(AST.JSExpressionBinary "<=" $1 $3)]}
                     | RelationalExpression '>=' ShiftExpression { [(AST.JSExpressionBinary ">=" $1 $3)]}
                     | RelationalExpression 'instanceof' ShiftExpression { [(AST.JSExpressionBinary " instanceof " $1 $3)]}
                     | RelationalExpression 'in' ShiftExpression { [(AST.JSExpressionBinary " in " $1 $3)]}

-- RelationalExpressionNoIn :                                  See 11.8
--        ShiftExpression
--        RelationalExpressionNoIn < ShiftExpression
--        RelationalExpressionNoIn > ShiftExpression
--        RelationalExpressionNoIn <= ShiftExpression
--        RelationalExpressionNoIn >= ShiftExpression
--        RelationalExpressionNoIn instanceof ShiftExpression
RelationalExpressionNoIn :: { [AST.JSNode] }
RelationalExpressionNoIn : ShiftExpression { $1 {- RelationalExpressionNoIn -}} 
                     | RelationalExpressionNoIn '<'  ShiftExpression { [(AST.JSExpressionBinary "<" $1 $3)]}
                     | RelationalExpressionNoIn '>'  ShiftExpression { [(AST.JSExpressionBinary ">" $1 $3)]}
                     | RelationalExpressionNoIn '<=' ShiftExpression { [(AST.JSExpressionBinary "<=" $1 $3)]}
                     | RelationalExpressionNoIn '>=' ShiftExpression { [(AST.JSExpressionBinary ">=" $1 $3)]}
                     | RelationalExpressionNoIn 'instanceof' ShiftExpression { [(AST.JSExpressionBinary " instanceof " $1 $3)]}

-- EqualityExpression :                                        See 11.9
--        RelationalExpression
--        EqualityExpression == RelationalExpression
--        EqualityExpression != RelationalExpression
--        EqualityExpression === RelationalExpression
--        EqualityExpression !== RelationalExpression
EqualityExpression :: { [AST.JSNode] }
EqualityExpression : RelationalExpression { $1 {- EqualityExpression -} } 
                   | EqualityExpression '=='  RelationalExpression { [(AST.JSExpressionBinary "==" $1 $3)]}
                   | EqualityExpression '!='  RelationalExpression { [(AST.JSExpressionBinary "!=" $1 $3)]}
                   | EqualityExpression '===' RelationalExpression { [(AST.JSExpressionBinary "===" $1 $3)]}
                   | EqualityExpression '!==' RelationalExpression { [(AST.JSExpressionBinary "!==" $1 $3)]}

-- EqualityExpressionNoIn :                                    See 11.9
--        RelationalExpressionNoIn
--        EqualityExpressionNoIn == RelationalExpressionNoIn
--        EqualityExpressionNoIn != RelationalExpressionNoIn
--        EqualityExpressionNoIn === RelationalExpressionNoIn
--        EqualityExpressionNoIn !== RelationalExpressionNoIn
EqualityExpressionNoIn :: { [AST.JSNode] }
EqualityExpressionNoIn : RelationalExpressionNoIn { $1 {- EqualityExpressionNoIn -} } 
                       | EqualityExpressionNoIn '=='  RelationalExpression { [(AST.JSExpressionBinary "==" $1 $3)]}
                       | EqualityExpressionNoIn '!='  RelationalExpression { [(AST.JSExpressionBinary "!=" $1 $3)]}
                       | EqualityExpressionNoIn '===' RelationalExpression { [(AST.JSExpressionBinary "===" $1 $3)]}
                       | EqualityExpressionNoIn '!==' RelationalExpression { [(AST.JSExpressionBinary "!==" $1 $3)]}

-- BitwiseANDExpression :                                      See 11.10
--        EqualityExpression
--        BitwiseANDExpression & EqualityExpression
BitwiseAndExpression :: { [AST.JSNode] }
BitwiseAndExpression : EqualityExpression { $1 {- BitwiseAndExpression -} } 
                     | BitwiseAndExpression '&' EqualityExpression { [(AST.JSExpressionBinary "&" $1 $3)]}

-- BitwiseANDExpressionNoIn :                                  See 11.10
--        EqualityExpressionNoIn
--        BitwiseANDExpressionNoIn & EqualityExpressionNoIn
BitwiseAndExpressionNoIn :: { [AST.JSNode] }
BitwiseAndExpressionNoIn : EqualityExpressionNoIn { $1 {- BitwiseAndExpression -} } 
                     | BitwiseAndExpressionNoIn '&' EqualityExpressionNoIn { [(AST.JSExpressionBinary "&" $1 $3)]}

-- BitwiseXORExpression :                                                                See 11.10
--        BitwiseANDExpression
--        BitwiseXORExpression ^ BitwiseANDExpression
BitwiseXOrExpression :: { [AST.JSNode] }
BitwiseXOrExpression : BitwiseAndExpression { $1 {- BitwiseXOrExpression -} } 
                     | BitwiseXOrExpression '^' BitwiseAndExpression { [(AST.JSExpressionBinary "^" $1 $3)]}

-- BitwiseXORExpressionNoIn :                                                            See 11.10
--        BitwiseANDExpressionNoIn
--        BitwiseXORExpressionNoIn ^ BitwiseANDExpressionNoIn
BitwiseXOrExpressionNoIn :: { [AST.JSNode] }
BitwiseXOrExpressionNoIn : BitwiseAndExpressionNoIn { $1 {- BitwiseXOrExpression -} } 
                         | BitwiseXOrExpressionNoIn '^' BitwiseAndExpressionNoIn { [(AST.JSExpressionBinary "^" $1 $3)]}

-- BitwiseORExpression :                                                                 See 11.10
--        BitwiseXORExpression
--        BitwiseORExpression | BitwiseXORExpression
BitwiseOrExpression :: { [AST.JSNode] }
BitwiseOrExpression : BitwiseXOrExpression { $1 {- BitwiseOrExpression -} } 
                    | BitwiseOrExpression '|' BitwiseXOrExpression { [(AST.JSExpressionBinary "|" $1 $3)]}

-- BitwiseORExpressionNoIn :                                                             See 11.10
--        BitwiseXORExpressionNoIn
--        BitwiseORExpressionNoIn | BitwiseXORExpressionNoIn
BitwiseOrExpressionNoIn :: { [AST.JSNode] }
BitwiseOrExpressionNoIn : BitwiseXOrExpressionNoIn { $1 {- BitwiseOrExpression -} } 
                        | BitwiseOrExpressionNoIn '|' BitwiseXOrExpressionNoIn { [(AST.JSExpressionBinary "|" $1 $3)]}

-- LogicalANDExpression :                                                                See 11.11
--        BitwiseORExpression
--        LogicalANDExpression && BitwiseORExpression
LogicalAndExpression :: { [AST.JSNode] }
LogicalAndExpression : BitwiseOrExpression { $1 {- LogicalAndExpression -} } 
                     | LogicalAndExpression '&&' BitwiseOrExpression { [(AST.JSExpressionBinary "&&" $1 $3)]}

-- LogicalANDExpressionNoIn :                                                            See 11.11
--        BitwiseORExpressionNoIn
--        LogicalANDExpressionNoIn && BitwiseORExpressionNoIn
LogicalAndExpressionNoIn :: { [AST.JSNode] }
LogicalAndExpressionNoIn : BitwiseOrExpressionNoIn { $1 {- LogicalAndExpression -} } 
                         | LogicalAndExpressionNoIn '&&' BitwiseOrExpressionNoIn { [(AST.JSExpressionBinary "&&" $1 $3)]}

-- LogicalORExpression :                                                                 See 11.11
--        LogicalANDExpression
--        LogicalORExpression || LogicalANDExpression
LogicalOrExpression :: { [AST.JSNode] }
LogicalOrExpression : LogicalAndExpression { $1 {- LogicalOrExpression -} } 
                    | LogicalOrExpression '||' LogicalAndExpression { [(AST.JSExpressionBinary "||" $1 $3)]}

-- LogicalORExpressionNoIn :                                                             See 11.11
--        LogicalANDExpressionNoIn
--        LogicalORExpressionNoIn || LogicalANDExpressionNoIn
LogicalOrExpressionNoIn :: { [AST.JSNode] }
LogicalOrExpressionNoIn : LogicalAndExpressionNoIn { $1 {- LogicalOrExpression -} } 
                        | LogicalOrExpressionNoIn '||' LogicalAndExpressionNoIn { [(AST.JSExpressionBinary "||" $1 $3)]}

-- ConditionalExpression :                                                               See 11.12
--        LogicalORExpression
--        LogicalORExpression ? AssignmentExpression : AssignmentExpression
ConditionalExpression :: { [AST.JSNode] }
ConditionalExpression : LogicalOrExpression { $1 {- ConditionalExpression -} }
                    | LogicalOrExpression '?' AssignmentExpression ':' AssignmentExpression 
                      { [AST.JSExpressionTernary $1 $3 $5] } 
                    
-- ConditionalExpressionNoIn :                                                           See 11.12
--        LogicalORExpressionNoIn
--        LogicalORExpressionNoIn ? AssignmentExpressionNoIn : AssignmentExpressionNoIn
ConditionalExpressionNoIn :: { [AST.JSNode] }
ConditionalExpressionNoIn : LogicalOrExpressionNoIn { $1 {- ConditionalExpression -} }
                          | LogicalOrExpressionNoIn '?' AssignmentExpressionNoIn ':' AssignmentExpressionNoIn
                            { [AST.JSExpressionTernary $1 $3 $5] } 
  
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
AssignmentOperator : 'assign' { AST.JSOperator (token_literal $1) }
                   | '='      { AST.JSOperator "=" }

-- Expression :                                                   See 11.14
--         AssignmentExpression
--         Expression , AssignmentExpression
Expression :: { AST.JSNode }
Expression : AssignmentExpression { AST.JSExpression $1 {- Expression -} } 
           | Expression ',' AssignmentExpression  { flattenExpression $1 $3 }

-- ExpressionNoIn :                                               See 11.14
--         AssignmentExpressionNoIn
--         ExpressionNoIn , AssignmentExpressionNoIn
ExpressionNoIn :: { AST.JSNode }
ExpressionNoIn : AssignmentExpressionNoIn { AST.JSExpression $1 {- Expression -} } 
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

StatementBlock :: { AST.JSNode }
StatementBlock : '{' '}'               { (AST.JSLiteral ";") }
               | '{' StatementList '}' { (if ($2 == AST.JSStatementList [AST.JSLiteral ";"]) then (AST.JSLiteral ";") else (AST.JSBlock $2)) }

-- Block :                                                        See 12.1
--         { StatementListopt }
Block :: { AST.JSNode }
Block : '{' '}'               { (AST.JSBlock (AST.JSStatementList [])) }
      | '{' StatementList '}' { (AST.JSBlock $2) }

-- StatementList :                                                See 12.1
--         Statement
--         StatementList Statement
StatementList :: { AST.JSNode }
StatementList : Statement               { (AST.JSStatementList [$1]) }
              | StatementList Statement { (combineStatements $1 $2) }

-- VariableStatement :                                            See 12.2
--         var VariableDeclarationList ;
VariableStatement :: { AST.JSNode }
VariableStatement : 'var'   VariableDeclarationList AutoSemi { AST.JSVariables "var" $2 }
                  | 'const' VariableDeclarationList AutoSemi { AST.JSVariables "const" $2 }

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
VariableDeclaration : Identifier              { (AST.JSVarDecl $1 [])}
                    | Identifier Initializer  { (AST.JSVarDecl $1 $2)}

-- VariableDeclarationNoIn :                                      See 12.2
--         Identifier InitialiserNoInopt
VariableDeclarationNoIn :: { AST.JSNode }
VariableDeclarationNoIn : Identifier InitializerNoIn { (AST.JSVarDecl $1 $2) }
                        | Identifier                 { (AST.JSVarDecl $1 []) }

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
EmptyStatement : ';' { (AST.JSLiteral ";") }

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
                       (if (length $6 == 1) then (AST.JSIfElse $3 $5 (head $6)) 
                                            else (AST.JSIfElse $3 (AST.JSBlock (AST.JSStatementList [$5])) (last $6))) 
                     else (AST.JSIf $3 $5)) }
                  
IfElseRest :: { [AST.JSNode] }                  
IfElseRest : 'else' Statement     { [$2] }
           |                      { [] } 

StatementSemi :: { AST.JSNode }
StatementSemi : StatementNoEmpty ';' { (AST.JSBlock (AST.JSStatementList [$1])) } 
              | StatementNoEmpty     { $1 {- StatementSemi -}}
              | ';'                  { AST.JSLiteral ";" }  


-- IterationStatement :                                                                     See 12.6
--         do Statement while ( Expression );
--         while ( Expression ) Statement
--         for (ExpressionNoInopt; Expressionopt ; Expressionopt ) Statement
--         for ( var VariableDeclarationListNoIn; Expressionopt ; Expressionopt ) Statement
--         for ( LeftHandSideExpression in Expression ) Statement
--         for ( var VariableDeclarationNoIn in Expression ) Statement
IterationStatement :: { AST.JSNode }
IterationStatement : 'do' Statement 'while' '(' Expression ')' AutoSemi { (AST.JSDoWhile $2 $5 $7) } 
                   | 'while' '(' Expression ')' Statement { (AST.JSWhile $3 $5) }
                   | 'for' '(' ExpressionNoInOpt ';' ExpressionOpt ';' ExpressionOpt ')' Statement { (AST.JSFor $3 $5 $7 $9) }
                   | 'for' '(' 'var' VariableDeclarationListNoIn ';' ExpressionOpt ';' ExpressionOpt ')' Statement 
                     { (AST.JSForVar $4 $6 $8 $10) }
                   | 'for' '(' LeftHandSideExpression 'in' Expression ')' Statement 
                     { (AST.JSForIn $3 $5 $7) }
                   | 'for' '(' 'var' VariableDeclarationNoIn 'in' Expression ')' Statement
                     { (AST.JSForVarIn $4 $6 $8) }

-- ContinueStatement :                                                                      See 12.7
--         continue [no LineTerminator here] Identifieropt ;
-- TODO: deal with [no LineTerminator here]
ContinueStatement :: { AST.JSNode }
ContinueStatement : 'continue' AutoSemi             { (AST.JSContinue [$2]) } 
                  | 'continue' Identifier AutoSemi  { (AST.JSContinue [$2,$3]) } 

-- BreakStatement :                                                                         See 12.8
--         break [no LineTerminator here] Identifieropt ;
-- TODO: deal with [no LineTerminator here]
BreakStatement :: { AST.JSNode }
BreakStatement : 'break' AutoSemi             { (AST.JSBreak [] [$2]) } 
               | 'break' Identifier AutoSemi  { (AST.JSBreak [$2] [$3]) } 

-- ReturnStatement :                                                                        See 12.9
--         return [no LineTerminator here] Expressionopt ;
-- TODO: deal with [no LineTerminator here]
ReturnStatement :: { AST.JSNode }
ReturnStatement : 'return' AutoSemi             { (AST.JSReturn [$2]) } 
                | 'return' Expression AutoSemi  { (AST.JSReturn [$2,$3]) } 

-- WithStatement :                                                                          See 12.10
--         with ( Expression ) Statement
WithStatement :: { AST.JSNode }
WithStatement : 'with' '(' Expression ')' Statement AutoSemi  { (AST.JSWith $3 [$5,$6]) }

-- SwitchStatement :                                                                        See 12.11
--         switch ( Expression ) CaseBlock
SwitchStatement :: { AST.JSNode }
SwitchStatement : 'switch' '(' Expression ')' CaseBlock { (AST.JSSwitch $3 $5) } 

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
CaseClause : 'case' Expression ':' StatementList  { (AST.JSCase $2 $4) }
           | 'case' Expression ':'                { (AST.JSCase $2 (AST.JSStatementList [])) }

-- DefaultClause :                                                            See 12.11
--        default : StatementListopt
DefaultClause :: { AST.JSNode }
DefaultClause : 'default' ':'                { (AST.JSDefault (AST.JSStatementList [])) }
              | 'default' ':' StatementList  { (AST.JSDefault $3) }

-- LabelledStatement :                                                        See 12.12
--        Identifier : Statement
LabelledStatement :: { AST.JSNode }
LabelledStatement : Identifier ':' Statement { (AST.JSLabelled $1 $3) }

-- ThrowStatement :                                                           See 12.13
--        throw [no LineTerminator here] Expression ;
-- TODO : sort out no LineTerminator here
--        Does it need a semi at the end?
ThrowStatement :: { AST.JSNode }
ThrowStatement : 'throw' Expression { (AST.JSThrow $2) }

-- Note: worked in updated syntax as per https://developer.mozilla.org/en/JavaScript/Reference/Statements/try...catch
--   i.e., 0 or more catches, then an optional finally
-- TryStatement :                                                             See 12.14
--        try Block Catch
--        try Block Finally
--        try Block Catch Finally
TryStatement :: { AST.JSNode }
TryStatement : 'try' Block Catches         { (AST.JSTry $2 $3)         {- TryStatement1 -} }
             | 'try' Block Finally         { (AST.JSTry $2 [$3])       {- TryStatement2 -} }
             | 'try' Block Catches Finally { (AST.JSTry $2 ($3++[$4])) {- TryStatement3 -} }

Catches :: { [AST.JSNode] }
Catches : Catch         { [$1]       {- Catches 1 -} }
        | Catches Catch { ($1++[$2]) {- Catches 2 -} }

-- Note: worked in updated syntax as per https://developer.mozilla.org/en/JavaScript/Reference/Statements/try...catch
-- <Catch> ::= 'catch' '(' Identifier ')' <Block>
--   becomes
-- <Catch> ::= 'catch' '(' Identifier ')' <Block>
--           | 'catch' '(' Identifier 'if' ConditionalExpression ')' <Block>
Catch :: { AST.JSNode }
Catch : 'catch' '(' Identifier ')' Block                 { (AST.JSCatch $3 [] $5) }
      | 'catch' '(' Identifier 'if' ConditionalExpression ')' Block  { (AST.JSCatch $3 $5 $7) }

-- Finally :                                                                  See 12.14
--        finally Block
Finally :: { AST.JSNode }
Finally : 'finally' Block { (AST.JSFinally $2) }

-- DebuggerStatement :                                                        See 12.15
--        debugger ;
DebuggerStatement :: { AST.JSNode }
DebuggerStatement : 'debugger' AutoSemi { (AST.JSLiteral "debugger") }

-- FunctionDeclaration :                                                      See clause 13
--        function Identifier ( FormalParameterListopt ) { FunctionBody }
FunctionDeclaration :: { AST.JSNode }
FunctionDeclaration : 'function' Identifier '(' FormalParameterList ')' '{' FunctionBody '}'
                      { (AST.JSFunction $2 $4 $7) }
                    | 'function' Identifier '(' ')' '{' FunctionBody '}'
                      { (AST.JSFunction $2 [] $6) }

-- FunctionExpression :                                                       See clause 13
--        function Identifieropt ( FormalParameterListopt ) { FunctionBody }
FunctionExpression :: { AST.JSNode }
FunctionExpression : 'function' IdentifierOpt '(' ')' '{' FunctionBody '}'                      { (AST.JSFunctionExpression $2 [] $6) }
                   | 'function' IdentifierOpt '(' FormalParameterList ')' '{' FunctionBody '}'  { (AST.JSFunctionExpression $2 $4 $7) }

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
FunctionBody : SourceElements { (AST.JSFunctionBody [$1]) }
             |                { (AST.JSFunctionBody []) } 

-- Program :                                                                  See clause 14
--        SourceElementsopt
Program :: { AST.JSNode }
Program : SourceElementsTop { $1 {- Program -}}

-- SourceElements :                                                           See clause 14
--        SourceElement
--        SourceElements SourceElement
SourceElements :: { AST.JSNode }
SourceElements : SourceElement                { (AST.JSSourceElements [$1]) }
               | SourceElements SourceElement { (combineSourceElements $1 $2) }

SourceElementsTop :: { AST.JSNode }
SourceElementsTop : SourceElement                   { (AST.JSSourceElementsTop [$1]) }
                  | SourceElementsTop SourceElement { (combineSourceElementsTop $1 $2) }

-- SourceElement :
--       Statement
--       FunctionDeclaration
SourceElement :: { AST.JSNode }
SourceElement : Statement            { $1 {- SourceElement1 -} }
              | FunctionDeclaration  { $1 {- SourceElement2 -} } 

{

combineSourceElements :: AST.JSNode -> AST.JSNode -> AST.JSNode
combineSourceElements (AST.JSSourceElements xs) x = (AST.JSSourceElements (xs++[x]) )

combineSourceElementsTop :: AST.JSNode -> AST.JSNode -> AST.JSNode
combineSourceElementsTop (AST.JSSourceElementsTop xs) x = (AST.JSSourceElementsTop (xs++[x]) )

combineStatements :: AST.JSNode -> AST.JSNode -> AST.JSNode
combineStatements (AST.JSStatementList xs) (AST.JSStatementList ys) = (AST.JSStatementList (xs++ys) )
combineStatements (AST.JSStatementList xs) y = (AST.JSStatementList (xs++[y]) )

parseError :: Token -> P a 
parseError = throwError . UnexpectedToken 

flattenExpression :: AST.JSNode -> [AST.JSNode] -> AST.JSNode
flattenExpression (AST.JSExpression xs) e = AST.JSExpression (xs++litComma++e)
                        where
                          litComma :: [AST.JSNode]
                          litComma = [(AST.JSLiteral ",")]


}

-- Set emacs mode
-- Local Variables: 
-- mode:haskell
-- End:             
