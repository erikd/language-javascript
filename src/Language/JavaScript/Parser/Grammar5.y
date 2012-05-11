{
module Language.JavaScript.Parser.Grammar5 (
    parseProgram
  , parseLiteral
  , parsePrimaryExpression
  , parseStatement
  ) where

import Data.Char
import Language.JavaScript.Parser.Lexer
import Language.JavaScript.Parser.ParseError
import Language.JavaScript.Parser.ParserMonad
import Language.JavaScript.Parser.SrcLocation
import Language.JavaScript.Parser.Token
import qualified Language.JavaScript.Parser.AST as AST

}

-- The name of the generated function to be exported from the module
%name parseProgram           Program
%name parseLiteral           LiteralMain
%name parsePrimaryExpression PrimaryExpressionMain
%name parseStatement         StatementMain

%tokentype { Token }
%error { parseError }
%monad { Alex } { >>= } { return }
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
     'octal'      { OctalToken {} }
     'string'     { StringToken {} }
     'regex'      { RegExToken {} }
     'assign'     { AssignToken {} }

     'future'     { FutureToken {} }

     'tail'       { TailToken {} }
     'eof'        { EOFToken {} }


%%

-- ---------------------------------------------------------------------
-- Sort out automatically inserted semi-colons

AutoSemi :: { AST.JSSemi }
AutoSemi : ';' { AST.JSSemi (AST.JSAnnot (ss $1) (gc $1)) }
         |     { AST.JSSemiAuto }

-- ---------------------------------------------------------------------

-- Helpers

LParen :: { AST.JSLParen }
LParen : '(' { AST.JSLParen (AST.JSAnnot (ss $1) (gc $1)) }

RParen :: { AST.JSRParen }
RParen : ')' { AST.JSRParen (AST.JSAnnot (ss $1) (gc $1)) }


LBrace :: { AST.JSLBrace }
LBrace : '{' { AST.JSLBrace (AST.JSAnnot (ss $1) (gc $1)) }

RBrace :: { AST.JSRBrace }
RBrace : '}' { AST.JSRBrace (AST.JSAnnot (ss $1) (gc $1)) }


LSquare :: { AST.JSLSquare }
LSquare : '[' { AST.JSLSquare (AST.JSAnnot (ss $1) (gc $1)) }

RSquare :: { AST.JSRSquare }
RSquare : ']' { AST.JSRSquare (AST.JSAnnot (ss $1) (gc $1)) }

Comma :: { AST.JSNode }
Comma : ',' { AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "," }

Colon :: { AST.JSNode }
Colon : ':' { AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) ":" }

Semi :: { AST.JSNode }
Semi : ';' { AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) ";" }

Dot :: { AST.JSNode }
Dot : '.' { AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "." }

Increment :: { AST.JSUnaryOp }
Increment : '++' { AST.JSUnaryOpIncr (AST.JSAnnot (ss $1) (gc $1)) }

Decrement :: { AST.JSUnaryOp }
Decrement : '--' { AST.JSUnaryOpDecr (AST.JSAnnot (ss $1) (gc $1)) }

Delete :: { AST.JSUnaryOp }
Delete : 'delete' { AST.JSUnaryOpDelete (AST.JSAnnot (ss $1) (gc $1)) }

Void :: { AST.JSUnaryOp }
Void : 'void' { AST.JSUnaryOpVoid (AST.JSAnnot (ss $1) (gc $1)) }

Typeof :: { AST.JSUnaryOp }
Typeof : 'typeof' { AST.JSUnaryOpTypeof (AST.JSAnnot (ss $1) (gc $1)) }

Plus :: { AST.JSBinOp }
Plus : '+' { AST.JSBinOpPlus (AST.JSAnnot (ss $1) (gc $1)) }

Minus :: { AST.JSBinOp }
Minus : '-' { AST.JSBinOpMinus (AST.JSAnnot (ss $1) (gc $1)) }

Tilde :: { AST.JSUnaryOp }
Tilde : '~' { AST.JSUnaryOpTilde (AST.JSAnnot (ss $1) (gc $1)) }

Not :: { AST.JSUnaryOp }
Not : '!' { AST.JSUnaryOpNot (AST.JSAnnot (ss $1) (gc $1)) }

Mul :: { AST.JSBinOp }
Mul : '*' { AST.JSBinOpTimes (AST.JSAnnot (ss $1) (gc $1)) }

Div :: { AST.JSBinOp }
Div : '/' { AST.JSBinOpDivide (AST.JSAnnot (ss $1) (gc $1)) }

Mod :: { AST.JSBinOp }
Mod : '%' { AST.JSBinOpMod (AST.JSAnnot (ss $1) (gc $1)) }

Lsh :: { AST.JSBinOp }
Lsh : '<<' { AST.JSBinOpLsh (AST.JSAnnot (ss $1) (gc $1)) }

Rsh :: { AST.JSBinOp }
Rsh : '>>' { AST.JSBinOpRsh (AST.JSAnnot (ss $1) (gc $1)) }

Ursh :: { AST.JSBinOp }
Ursh : '>>>' { AST.JSBinOpUrsh (AST.JSAnnot (ss $1) (gc $1)) }

Le :: { AST.JSBinOp }
Le : '<=' { AST.JSBinOpLe (AST.JSAnnot (ss $1) (gc $1)) }

Lt :: { AST.JSBinOp }
Lt : '<' { AST.JSBinOpLt (AST.JSAnnot (ss $1) (gc $1)) }

Ge :: { AST.JSBinOp }
Ge : '>=' { AST.JSBinOpGe (AST.JSAnnot (ss $1) (gc $1)) }

Gt :: { AST.JSBinOp }
Gt : '>' { AST.JSBinOpGt (AST.JSAnnot (ss $1) (gc $1)) }

In :: { AST.JSBinOp }
In : 'in' { AST.JSBinOpIn (AST.JSAnnot (ss $1) (gc $1)) }

Instanceof :: { AST.JSBinOp }
Instanceof : 'instanceof' { AST.JSBinOpInstanceOf (AST.JSAnnot (ss $1) (gc $1)) }

StrictEq :: { AST.JSBinOp }
StrictEq : '===' { AST.JSBinOpStrictEq (AST.JSAnnot (ss $1) (gc $1)) }

Equal :: { AST.JSBinOp }
Equal : '==' { AST.JSBinOpEq (AST.JSAnnot (ss $1) (gc $1)) }

StrictNe :: { AST.JSBinOp }
StrictNe : '!==' { AST.JSBinOpStrictNeq (AST.JSAnnot (ss $1) (gc $1)) }

Ne :: { AST.JSBinOp }
Ne : '!=' { AST.JSBinOpNeq (AST.JSAnnot (ss $1) (gc $1))}

Or :: { AST.JSBinOp }
Or : '||' { AST.JSBinOpOr (AST.JSAnnot (ss $1) (gc $1)) }

And :: { AST.JSBinOp }
And : '&&' { AST.JSBinOpAnd (AST.JSAnnot (ss $1) (gc $1)) }

BitOr :: { AST.JSBinOp }
BitOr : '|' { AST.JSBinOpBitOr (AST.JSAnnot (ss $1) (gc $1)) }

BitAnd :: { AST.JSBinOp }
BitAnd : '&' { AST.JSBinOpBitAnd (AST.JSAnnot (ss $1) (gc $1)) }

BitXor :: { AST.JSBinOp }
BitXor : '^' { AST.JSBinOpBitXor (AST.JSAnnot (ss $1) (gc $1))}

Hook :: { AST.JSNode }
Hook : '?' { AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "?" }

SimpleAssign :: { AST.JSNode }
SimpleAssign : '=' { AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "=" }

Assign :: { AST.JSNode }
Assign : 'assign' { AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) (token_literal $1) }

Var :: { AST.JSNode }
Var : 'var' { AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "var" }

Const :: { AST.JSNode }
Const : 'const' { AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "const" }

If :: { AST.JSNode }
If : 'if' { AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "if" }

Else :: { AST.JSNode }
Else : 'else' { AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "else" }

Do :: { AST.JSNode }
Do : 'do' { AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "do" }

While :: { AST.JSNode }
While : 'while' { AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "while" }

For :: { AST.JSNode }
For : 'for' { AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "for" }

Continue :: { [AST.JSNode] -> AST.JSSemi -> AST.JSNode }
Continue : 'continue' { AST.JSContinue (AST.JSAnnot (ss $1) (gc $1)) }

Break :: { [AST.JSNode] -> AST.JSSemi -> AST.JSNode }
Break : 'break' { AST.JSBreak (AST.JSAnnot (ss $1) (gc $1)) }

Return :: { [AST.JSNode] -> AST.JSSemi -> AST.JSNode }
Return : 'return' { AST.JSReturn (AST.JSAnnot (ss $1) (gc $1)) }

With :: { AST.JSLParen -> AST.JSNode -> AST.JSRParen -> AST.JSNode -> AST.JSSemi -> AST.JSNode }
With : 'with' { AST.JSWith (AST.JSAnnot (ss $1) (gc $1)) }

Switch :: { AST.JSLParen -> AST.JSNode -> AST.JSRParen -> AST.JSNode -> AST.JSNode }
Switch : 'switch' { AST.JSSwitch (AST.JSAnnot (ss $1) (gc $1)) }

Case :: { AST.JSNode -> AST.JSNode -> [AST.JSNode] -> AST.JSNode }
Case : 'case' { AST.JSCase (AST.JSAnnot (ss $1) (gc $1)) }

Default :: { AST.JSNode -> [AST.JSNode] -> AST.JSNode }
Default : 'default' { AST.JSDefault (AST.JSAnnot (ss $1) (gc $1)) }

Throw :: { AST.JSNode -> AST.JSNode }
Throw : 'throw' { AST.JSThrow (AST.JSAnnot (ss $1) (gc $1)) }

Try :: { AST.JSNode -> [AST.JSNode] -> AST.JSNode }
Try : 'try' { AST.JSTry (AST.JSAnnot (ss $1) (gc $1)) }

CatchL :: { AST.JSLParen -> AST.JSNode -> [AST.JSNode] -> AST.JSRParen -> AST.JSNode -> AST.JSNode }
CatchL : 'catch' { AST.JSCatch (AST.JSAnnot (ss $1) (gc $1)) }

FinallyL :: { AST.JSNode -> AST.JSNode }
FinallyL : 'finally' { AST.JSFinally (AST.JSAnnot (ss $1) (gc $1)) }

Function :: { AST.JSNode }
Function : 'function' { AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "function" }

Eof :: { AST.JSNode }
Eof : 'tail' { AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "" }

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
NullLiteral : 'null' { AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "null" }

BooleanLiteral :: { AST.JSNode }
BooleanLiteral : 'true'  { AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "true" }
               | 'false' { AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "false" }

-- <Numeric Literal> ::= DecimalLiteral
--                     | HexIntegerLiteral
--                     | OctalLiteral
NumericLiteral :: { AST.JSNode }
NumericLiteral : 'decimal'    { AST.JSDecimal (AST.JSAnnot (ss $1) (gc $1)) (token_literal $1) }
               | 'hexinteger' { AST.JSHexInteger (AST.JSAnnot (ss $1) (gc $1)) (token_literal $1) }
               | 'octal'      { AST.JSOctal (AST.JSAnnot (ss $1) (gc $1)) (token_literal $1) }

StringLiteral :: { AST.JSNode }
StringLiteral : 'string'  { AST.JSStringLiteral (AST.JSAnnot (ss $1) (gc $1)) (token_delimiter $1) (token_literal $1) }

-- <Regular Expression Literal> ::= RegExp
RegularExpressionLiteral :: { AST.JSNode }
RegularExpressionLiteral : 'regex' { AST.JSRegEx (AST.JSAnnot (ss $1) (gc $1)) (token_literal $1) }

-- PrimaryExpression :                                                   See 11.1
--        this
--        Identifier
--        Literal
--        ArrayLiteral
--        ObjectLiteral
--        ( Expression )
PrimaryExpression :: { AST.JSNode }
PrimaryExpression : 'this'                   { AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "this" }
                  | Identifier               { $1 {- PrimaryExpression1 -}}
                  | Literal                  { $1 {- PrimaryExpression2 -}}
                  | ArrayLiteral             { $1 {- PrimaryExpression3 -}}
                  | ObjectLiteral            { $1 {- PrimaryExpression4 -}}
                  | LParen Expression RParen { AST.JSExpressionParen $1 $2 $3 }

-- Identifier ::                                                            See 7.6
--         IdentifierName but not ReservedWord
-- IdentifierName ::                                                        See 7.6
--         IdentifierStart
--         IdentifierName IdentifierPart
Identifier :: { AST.JSNode }
Identifier : 'ident' {  (AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) (token_literal $1))}
           | 'get'   {  (AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "get")}
           | 'set'   {  (AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "set")}

-- TODO: make this include any reserved word too, including future ones
IdentifierName :: { AST.JSNode }
IdentifierName : Identifier {$1}
             | 'break'      { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "break" }
             | 'case'       { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "case" }
             | 'catch'      { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "catch" }
             | 'const'      { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "const" }
             | 'continue'   { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "continue" }
             | 'debugger'   { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "debugger" }
             | 'default'    { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "default" }
             | 'delete'     { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "delete" }
             | 'do'         { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "do" }
             | 'else'       { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "else" }
             | 'enum'       { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "enum" }
             | 'false'      { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "false" }
             | 'finally'    { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "finally" }
             | 'for'        { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "for" }
             | 'function'   { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "function" }
             | 'get'        { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "get" }
             | 'if'         { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "if" }
             | 'in'         { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "in" }
             | 'instanceof' { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "instanceof" }
             | 'new'        { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "new" }
             | 'null'       { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "null" }
             | 'return'     { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "return" }
             | 'set'        { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "set" }
             | 'switch'     { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "switch" }
             | 'this'       { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "this" }
             | 'throw'      { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "throw" }
             | 'true'       { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "true" }
             | 'try'        { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "try" }
             | 'typeof'     { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "typeof" }
             | 'var'        { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "var" }
             | 'void'       { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "void" }
             | 'while'      { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "while" }
             | 'with'       { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "with" }
             | 'future'     { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) (token_literal $1) }



-- ArrayLiteral :                                                        See 11.1.4
--        [ Elisionopt ]
--        [ ElementList ]
--        [ ElementList , Elisionopt ]
ArrayLiteral :: { AST.JSNode }
ArrayLiteral : LSquare RSquare                           { AST.JSArrayLiteral $1 [] $2 }
             | LSquare Elision RSquare                   { AST.JSArrayLiteral $1 $2 $3 }
             | LSquare ElementList RSquare               { AST.JSArrayLiteral $1 $2 $3 }
             | LSquare ElementList Comma Elision RSquare { AST.JSArrayLiteral $1 ($2++[$3]++$4) $5 }
             | LSquare ElementList Comma RSquare         { AST.JSArrayLiteral $1 ($2++[$3])     $4 }



-- ElementList :                                                         See 11.1.4
--        Elisionopt AssignmentExpression
--        ElementList , Elisionopt AssignmentExpression
ElementList :: { [AST.JSNode] }
ElementList : Elision AssignmentExpression                 { (($1)++($2)) {- ElementList -}}
            | AssignmentExpression                         { $1           {- ElementList -}}
            | ElementList Comma Elision AssignmentExpression { (($1)++[AST.JSElision AST.JSNoAnnot $2]++($3)++($4)) {- ElementList -}}
            | ElementList Comma AssignmentExpression         { (($1)++[AST.JSElision AST.JSNoAnnot $2]++($3)) {- ElementList -}}


-- Elision :                                                             See 11.1.4
--        ,
--        Elision ,
Elision :: { [AST.JSNode] }
Elision : Comma         { [      AST.JSElision AST.JSNoAnnot $1] }
        | Elision Comma { $1 ++ [AST.JSElision AST.JSNoAnnot $2] }

-- ObjectLiteral :                                                       See 11.1.5
--        { }
--        { PropertyNameAndValueList }
--        { PropertyNameAndValueList , }
ObjectLiteral :: { AST.JSNode }
ObjectLiteral : LBrace RBrace                                { AST.JSObjectLiteral $1 [] $2          }
              | LBrace PropertyNameandValueList RBrace       { AST.JSObjectLiteral $1 $2 $3          }
              | LBrace PropertyNameandValueList Comma RBrace { AST.JSObjectLiteral $1 ($2++[$3]) $4  }

-- <Property Name and Value List> ::= <Property Name> ':' <Assignment Expression>
--                                  | <Property Name and Value List> ',' <Property Name> ':' <Assignment Expression>

-- Seems we can have function declarations in the value part too
-- PropertyNameAndValueList :                                            See 11.1.5
--        PropertyAssignment
--        PropertyNameAndValueList , PropertyAssignment
PropertyNameandValueList :: { [ AST.JSNode ] }
PropertyNameandValueList : PropertyAssignment                              { [$1] {- PropertyNameandValueList1 -} }
                         | PropertyNameandValueList Comma PropertyAssignment { ($1++[$2]++[$3]) {- PropertyNameandValueList2 -} }

-- PropertyAssignment :                                                  See 11.1.5
--        PropertyName : AssignmentExpression
--        get PropertyName() { FunctionBody }
--        set PropertyName( PropertySetParameterList ) { FunctionBody }
-- TODO: not clear if get/set are keywords, or just used in a specific context. Puzzling.
PropertyAssignment :: { AST.JSNode }
PropertyAssignment : PropertyName Colon AssignmentExpression { AST.JSPropertyNameandValue AST.JSNoAnnot $1 $2 $3 }
                   -- Should be "get" in next, but is not a Token
                   | 'get' PropertyName LParen RParen FunctionBody
                       { AST.JSPropertyAccessor AST.JSNoAnnot (AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "get") $2 $3 [] $4 $5 }
                   -- Should be "set" in next, but is not a Token
                   | 'set' PropertyName LParen PropertySetParameterList RParen FunctionBody
                       { AST.JSPropertyAccessor AST.JSNoAnnot (AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "set") $2 $3 [$4] $5 $6 }

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
                 | MemberExpression LSquare Expression RSquare { [AST.JSMemberSquare $1 $2 $3 $4] }
                 | MemberExpression Dot IdentifierName         { [AST.JSMemberDot $1 $2 $3] }
                 | 'new' MemberExpression Arguments            { (((AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "new")):$2)++[$3] }

-- NewExpression :                                              See 11.2
--        MemberExpression
--        new NewExpression
NewExpression :: { [AST.JSNode] }
NewExpression : MemberExpression    { $1 {- NewExpression -}}
              | 'new' NewExpression { (AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "new"):$2 }

-- CallExpression :                                             See 11.2
--        MemberExpression Arguments
--        CallExpression Arguments
--        CallExpression [ Expression ]
--        CallExpression . IdentifierName
CallExpression :: { [AST.JSNode] }
CallExpression : MemberExpression Arguments        { $1++[$2] {- CallExpression -} }
               | CallExpression Arguments          { $1++[AST.JSCallExpression [] [$2] []] }
               | CallExpression LSquare Expression RSquare { $1++[AST.JSCallExpressionSquare $2 [$3] $4] }
               | CallExpression Dot IdentifierName { $1++[ AST.JSCallExpressionDot $2 [$3] ] }

-- Arguments :                                                  See 11.2
--        ()
--        ( ArgumentList )
Arguments :: { AST.JSNode }
Arguments : LParen RParen               { AST.JSArguments $1 [] $2 }
          | LParen ArgumentList RParen  { AST.JSArguments $1 $2 $3 }

-- ArgumentList :                                               See 11.2
--        AssignmentExpression
--        ArgumentList , AssignmentExpression
ArgumentList :: { [AST.JSNode] }
ArgumentList : AssignmentExpression { $1 {- ArgumentList -}}
             | ArgumentList Comma AssignmentExpression { $1++[$2]++$3 {- ArgumentList2 -} }

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
                  | PostfixExpression Increment {[AST.JSExpressionPostfix $1 $2]}
                  | PostfixExpression Decrement {[AST.JSExpressionPostfix $1 $2]}

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
                | Delete    UnaryExpression { ((AST.JSUnary $1):$2) }
                | Void      UnaryExpression { ((AST.JSUnary $1):$2) }
                | Typeof    UnaryExpression { ((AST.JSUnary $1):$2) }
                | Increment UnaryExpression { ((AST.JSUnary $1):$2) }
                | Decrement UnaryExpression { ((AST.JSUnary $1):$2) }
                | Plus      UnaryExpression { ((AST.JSUnary (mkUnary $1)):$2) }
                | Minus     UnaryExpression { ((AST.JSUnary (mkUnary $1)):$2) }
                | Tilde     UnaryExpression { ((AST.JSUnary $1):$2) }
                | Not       UnaryExpression { ((AST.JSUnary $1):$2) }

-- MultiplicativeExpression :                                   See 11.5
--        UnaryExpression
--        MultiplicativeExpression * UnaryExpression
--        MultiplicativeExpression / UnaryExpression
--        MultiplicativeExpression % UnaryExpression
MultiplicativeExpression :: { [AST.JSNode] }
MultiplicativeExpression : UnaryExpression { $1 {- MultiplicativeExpression -}}
                         | MultiplicativeExpression Mul UnaryExpression { [AST.JSExpressionBinary {- * -} $1 $2 $3]}
                         | MultiplicativeExpression Div UnaryExpression { [AST.JSExpressionBinary {- / -} $1 $2 $3]}
                         | MultiplicativeExpression Mod UnaryExpression { [AST.JSExpressionBinary {- % -} $1 $2 $3]}

-- AdditiveExpression :                                        See 11.6
--        MultiplicativeExpression
--        AdditiveExpression + MultiplicativeExpression
--        AdditiveExpression - MultiplicativeExpression
AdditiveExpression :: { [AST.JSNode] }
AdditiveExpression : AdditiveExpression Plus  MultiplicativeExpression { [AST.JSExpressionBinary {- + -} $1 $2 $3]}
                   | AdditiveExpression Minus MultiplicativeExpression { [AST.JSExpressionBinary {- - -} $1 $2 $3]}
                   | MultiplicativeExpression { $1 {- (goRegExp $1)-} {- AdditiveExpression -} }

-- ShiftExpression :                                           See 11.7
--        AdditiveExpression
--        ShiftExpression << AdditiveExpression
--        ShiftExpression >> AdditiveExpression
--        ShiftExpression >>> AdditiveExpression
ShiftExpression :: { [AST.JSNode] }
ShiftExpression : ShiftExpression Lsh  AdditiveExpression { [AST.JSExpressionBinary {- << -}  $1 $2 $3]}
                | ShiftExpression Rsh  AdditiveExpression { [AST.JSExpressionBinary {- >> -}  $1 $2 $3]}
                | ShiftExpression Ursh AdditiveExpression { [AST.JSExpressionBinary {- >>> -} $1 $2 $3]}
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
                     | RelationalExpression Lt  ShiftExpression { [AST.JSExpressionBinary {- < -}  $1 $2 $3]}
                     | RelationalExpression Gt  ShiftExpression { [AST.JSExpressionBinary {- > -}  $1 $2 $3]}
                     | RelationalExpression Le  ShiftExpression { [AST.JSExpressionBinary {- <= -} $1 $2 $3]}
                     | RelationalExpression Ge  ShiftExpression { [AST.JSExpressionBinary {- >= -} $1 $2 $3]}
                     | RelationalExpression Instanceof ShiftExpression { [AST.JSExpressionBinary {-  instanceof -} $1 $2 $3]}
                     | RelationalExpression In         ShiftExpression { [AST.JSExpressionBinary {-  in         -} $1 $2 $3]}

-- RelationalExpressionNoIn :                                  See 11.8
--        ShiftExpression
--        RelationalExpressionNoIn < ShiftExpression
--        RelationalExpressionNoIn > ShiftExpression
--        RelationalExpressionNoIn <= ShiftExpression
--        RelationalExpressionNoIn >= ShiftExpression
--        RelationalExpressionNoIn instanceof ShiftExpression
RelationalExpressionNoIn :: { [AST.JSNode] }
RelationalExpressionNoIn : ShiftExpression { $1 {- RelationalExpressionNoIn -}}
                     | RelationalExpressionNoIn Lt  ShiftExpression { [AST.JSExpressionBinary {- < -}  $1 $2 $3]}
                     | RelationalExpressionNoIn Gt  ShiftExpression { [AST.JSExpressionBinary {- > -}  $1 $2 $3]}
                     | RelationalExpressionNoIn Le  ShiftExpression { [AST.JSExpressionBinary {- <= -} $1 $2 $3]}
                     | RelationalExpressionNoIn Ge  ShiftExpression { [AST.JSExpressionBinary {- >= -} $1 $2 $3]}
                     | RelationalExpressionNoIn Instanceof ShiftExpression { [AST.JSExpressionBinary {-  instanceof  -} $1 $2 $3]}

-- EqualityExpression :                                        See 11.9
--        RelationalExpression
--        EqualityExpression == RelationalExpression
--        EqualityExpression != RelationalExpression
--        EqualityExpression === RelationalExpression
--        EqualityExpression !== RelationalExpression
EqualityExpression :: { [AST.JSNode] }
EqualityExpression : RelationalExpression { $1 {- EqualityExpression -} }
                   | EqualityExpression Equal    RelationalExpression { [AST.JSExpressionBinary {- == -}  $1 $2 $3]}
                   | EqualityExpression Ne       RelationalExpression { [AST.JSExpressionBinary {- != -}  $1 $2 $3]}
                   | EqualityExpression StrictEq RelationalExpression { [AST.JSExpressionBinary {- === -} $1 $2 $3]}
                   | EqualityExpression StrictNe RelationalExpression { [AST.JSExpressionBinary {- !== -} $1 $2 $3]}

-- EqualityExpressionNoIn :                                    See 11.9
--        RelationalExpressionNoIn
--        EqualityExpressionNoIn == RelationalExpressionNoIn
--        EqualityExpressionNoIn != RelationalExpressionNoIn
--        EqualityExpressionNoIn === RelationalExpressionNoIn
--        EqualityExpressionNoIn !== RelationalExpressionNoIn
EqualityExpressionNoIn :: { [AST.JSNode] }
EqualityExpressionNoIn : RelationalExpressionNoIn { $1 {- EqualityExpressionNoIn -} }
                       | EqualityExpressionNoIn Equal    RelationalExpression { [AST.JSExpressionBinary {- == -}  $1 $2 $3]}
                       | EqualityExpressionNoIn Ne       RelationalExpression { [AST.JSExpressionBinary {- != -}  $1 $2 $3]}
                       | EqualityExpressionNoIn StrictEq RelationalExpression { [AST.JSExpressionBinary {- === -} $1 $2 $3]}
                       | EqualityExpressionNoIn StrictNe RelationalExpression { [AST.JSExpressionBinary {- !== -} $1 $2 $3]}

-- BitwiseANDExpression :                                      See 11.10
--        EqualityExpression
--        BitwiseANDExpression & EqualityExpression
BitwiseAndExpression :: { [AST.JSNode] }
BitwiseAndExpression : EqualityExpression { $1 {- BitwiseAndExpression -} }
                     | BitwiseAndExpression BitAnd EqualityExpression { [AST.JSExpressionBinary {- & -} $1 $2 $3]}

-- BitwiseANDExpressionNoIn :                                  See 11.10
--        EqualityExpressionNoIn
--        BitwiseANDExpressionNoIn & EqualityExpressionNoIn
BitwiseAndExpressionNoIn :: { [AST.JSNode] }
BitwiseAndExpressionNoIn : EqualityExpressionNoIn { $1 {- BitwiseAndExpression -} }
                     | BitwiseAndExpressionNoIn BitAnd EqualityExpressionNoIn { [AST.JSExpressionBinary {- & -} $1 $2 $3]}

-- BitwiseXORExpression :                                                                See 11.10
--        BitwiseANDExpression
--        BitwiseXORExpression ^ BitwiseANDExpression
BitwiseXOrExpression :: { [AST.JSNode] }
BitwiseXOrExpression : BitwiseAndExpression { $1 {- BitwiseXOrExpression -} }
                     | BitwiseXOrExpression BitXor BitwiseAndExpression { [AST.JSExpressionBinary {- ^ -} $1 $2 $3]}

-- BitwiseXORExpressionNoIn :                                                            See 11.10
--        BitwiseANDExpressionNoIn
--        BitwiseXORExpressionNoIn ^ BitwiseANDExpressionNoIn
BitwiseXOrExpressionNoIn :: { [AST.JSNode] }
BitwiseXOrExpressionNoIn : BitwiseAndExpressionNoIn { $1 {- BitwiseXOrExpression -} }
                         | BitwiseXOrExpressionNoIn BitXor BitwiseAndExpressionNoIn { [AST.JSExpressionBinary {- ^ -} $1 $2 $3]}

-- BitwiseORExpression :                                                                 See 11.10
--        BitwiseXORExpression
--        BitwiseORExpression | BitwiseXORExpression
BitwiseOrExpression :: { [AST.JSNode] }
BitwiseOrExpression : BitwiseXOrExpression { $1 {- BitwiseOrExpression -} }
                    | BitwiseOrExpression BitOr BitwiseXOrExpression { [AST.JSExpressionBinary {- | -} $1 $2 $3]}

-- BitwiseORExpressionNoIn :                                                             See 11.10
--        BitwiseXORExpressionNoIn
--        BitwiseORExpressionNoIn | BitwiseXORExpressionNoIn
BitwiseOrExpressionNoIn :: { [AST.JSNode] }
BitwiseOrExpressionNoIn : BitwiseXOrExpressionNoIn { $1 {- BitwiseOrExpression -} }
                        | BitwiseOrExpressionNoIn BitOr BitwiseXOrExpressionNoIn { [AST.JSExpressionBinary {- | -} $1 $2 $3]}

-- LogicalANDExpression :                                                                See 11.11
--        BitwiseORExpression
--        LogicalANDExpression && BitwiseORExpression
LogicalAndExpression :: { [AST.JSNode] }
LogicalAndExpression : BitwiseOrExpression { $1 {- LogicalAndExpression -} }
                     | LogicalAndExpression And BitwiseOrExpression { [AST.JSExpressionBinary {- && -} $1 $2 $3]}

-- LogicalANDExpressionNoIn :                                                            See 11.11
--        BitwiseORExpressionNoIn
--        LogicalANDExpressionNoIn && BitwiseORExpressionNoIn
LogicalAndExpressionNoIn :: { [AST.JSNode] }
LogicalAndExpressionNoIn : BitwiseOrExpressionNoIn { $1 {- LogicalAndExpression -} }
                         | LogicalAndExpressionNoIn And BitwiseOrExpressionNoIn { [AST.JSExpressionBinary {- && -} $1 $2 $3]}

-- LogicalORExpression :                                                                 See 11.11
--        LogicalANDExpression
--        LogicalORExpression || LogicalANDExpression
LogicalOrExpression :: { [AST.JSNode] }
LogicalOrExpression : LogicalAndExpression { $1 {- LogicalOrExpression -} }
                    | LogicalOrExpression Or LogicalAndExpression { [AST.JSExpressionBinary {- || -} $1 $2 $3]}

-- LogicalORExpressionNoIn :                                                             See 11.11
--        LogicalANDExpressionNoIn
--        LogicalORExpressionNoIn || LogicalANDExpressionNoIn
LogicalOrExpressionNoIn :: { [AST.JSNode] }
LogicalOrExpressionNoIn : LogicalAndExpressionNoIn { $1 {- LogicalOrExpression -} }
                        | LogicalOrExpressionNoIn Or LogicalAndExpressionNoIn { [AST.JSExpressionBinary {- || -} $1 $2 $3]}

-- ConditionalExpression :                                                               See 11.12
--        LogicalORExpression
--        LogicalORExpression ? AssignmentExpression : AssignmentExpression
ConditionalExpression :: { [AST.JSNode] }
ConditionalExpression : LogicalOrExpression { $1 {- ConditionalExpression -} }
                      | LogicalOrExpression Hook AssignmentExpression Colon AssignmentExpression
                        { [AST.JSExpressionTernary $1 $2 $3 $4 $5] }

-- ConditionalExpressionNoIn :                                                           See 11.12
--        LogicalORExpressionNoIn
--        LogicalORExpressionNoIn ? AssignmentExpressionNoIn : AssignmentExpressionNoIn
ConditionalExpressionNoIn :: { [AST.JSNode] }
ConditionalExpressionNoIn : LogicalOrExpressionNoIn { $1 {- ConditionalExpression -} }
                          | LogicalOrExpressionNoIn Hook AssignmentExpressionNoIn Colon AssignmentExpressionNoIn
                            { [AST.JSExpressionTernary $1 $2 $3 $4 $5] }

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
AssignmentOperator : Assign       { AST.JSOperator $1 }
                   | SimpleAssign { AST.JSOperator $1 }

-- Expression :                                                   See 11.14
--         AssignmentExpression
--         Expression , AssignmentExpression
Expression :: { AST.JSNode }
Expression : AssignmentExpression { AST.JSExpression $1 {- Expression -} }
           | Expression Comma AssignmentExpression  { AST.JSExpression ($1:[$2]++$3) {- Expression2 -} }

-- ExpressionNoIn :                                               See 11.14
--         AssignmentExpressionNoIn
--         ExpressionNoIn , AssignmentExpressionNoIn
ExpressionNoIn :: { AST.JSNode }
ExpressionNoIn : AssignmentExpressionNoIn { AST.JSExpression $1 {- ExpressionNoIn -} }
               | ExpressionNoIn Comma AssignmentExpressionNoIn  { AST.JSExpression ($1:[$2]++$3) {- ExpressionNoIn2 -} }

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
StatementNoEmpty : StatementBlock      { $1 {- StatementNoEmpty1 -}}
                 | VariableStatement   { $1 {- StatementNoEmpty2 -}}
                 | ExpressionStatement { $1 {- StatementNoEmpty4 -}}
                 | IfStatement         { $1 {- StatementNoEmpty5 -}}
                 | IterationStatement  { $1 {- StatementNoEmpty6 -}}
                 | ContinueStatement   { $1 {- StatementNoEmpty7 -}}
                 | BreakStatement      { $1 {- StatementNoEmpty8 -}}
                 | ReturnStatement     { $1 {- StatementNoEmpty9 -}}
                 | WithStatement       { $1 {- StatementNoEmpty10 -}}
                 | LabelledStatement   { $1 {- StatementNoEmpty11 -}}
                 | SwitchStatement     { $1 {- StatementNoEmpty12 -}}
                 | ThrowStatement      { $1 {- StatementNoEmpty13 -}}
                 | TryStatement        { $1 {- StatementNoEmpty14 -}}
                 | DebuggerStatement   { $1 {- StatementNoEmpty15 -}}


StatementBlock :: { AST.JSNode }
StatementBlock : LBrace RBrace               { AST.JSBlock $1 [] $2 }
               | LBrace StatementList RBrace { AST.JSBlock $1 $2 $3 }

-- Block :                                                        See 12.1
--         { StatementListopt }
Block :: { AST.JSNode }
Block : LBrace RBrace               { AST.JSBlock $1 [] $2 }
      | LBrace StatementList RBrace { AST.JSBlock $1 $2 $3 }

-- StatementList :                                                See 12.1
--         Statement
--         StatementList Statement
StatementList :: { [AST.JSNode] }
StatementList : Statement               { [$1]       {- StatementList1 -} }
              | StatementList Statement { ($1++[$2]) {- StatementList2 -} }

-- VariableStatement :                                            See 12.2
--         var VariableDeclarationList ;
VariableStatement :: { AST.JSNode }
VariableStatement : Var   VariableDeclarationList AutoSemi { AST.JSVariables AST.JSNoAnnot $1 $2 $3 }
                  | Const VariableDeclarationList AutoSemi { AST.JSVariables AST.JSNoAnnot $1 $2 $3 }

-- VariableDeclarationList :                                      See 12.2
--         VariableDeclaration
--         VariableDeclarationList , VariableDeclaration
VariableDeclarationList :: { [AST.JSNode] }
VariableDeclarationList : VariableDeclaration { [$1] {- VariableDeclarationList -}}
                        | VariableDeclarationList Comma VariableDeclaration { ($1++[$2]++[$3]) {- VariableDeclarationList -}}

-- VariableDeclarationListNoIn :                                  See 12.2
--         VariableDeclarationNoIn
--         VariableDeclarationListNoIn , VariableDeclarationNoIn
VariableDeclarationListNoIn :: { [AST.JSNode] }
VariableDeclarationListNoIn : VariableDeclarationNoIn { [$1] {- VariableDeclarationList -}}
                            | VariableDeclarationListNoIn Comma VariableDeclarationNoIn { ($1++[$2]++[$3]) {- VariableDeclarationListNoIn -}}

-- VariableDeclaration :                                          See 12.2
--         Identifier Initialiseropt
VariableDeclaration :: { AST.JSNode }
VariableDeclaration : Identifier              { AST.JSVarDecl AST.JSNoAnnot $1 [] }
                    | Identifier Initializer  { AST.JSVarDecl AST.JSNoAnnot $1 $2 }

-- VariableDeclarationNoIn :                                      See 12.2
--         Identifier InitialiserNoInopt
VariableDeclarationNoIn :: { AST.JSNode }
VariableDeclarationNoIn : Identifier InitializerNoIn { AST.JSVarDecl AST.JSNoAnnot $1 $2 }
                        | Identifier                 { AST.JSVarDecl AST.JSNoAnnot $1 [] }

-- Initialiser :                                                                            See 12.2
--         = AssignmentExpression
Initializer :: { [AST.JSNode] }
Initializer : SimpleAssign AssignmentExpression { $1:$2 {- Initializer -} }

-- InitialiserNoIn :                                                                        See 12.2
--         = AssignmentExpressionNoIn
InitializerNoIn :: { [AST.JSNode] }
InitializerNoIn : SimpleAssign AssignmentExpressionNoIn { $1:$2 {- InitializerNoIn -}}

-- EmptyStatement :                                                                         See 12.3
--         ;
EmptyStatement :: { AST.JSNode }
EmptyStatement : Semi { $1 }

-- ExpressionStatement :                                                                    See 12.4
--         [lookahead not in {{, function}] Expression  ;
-- TODO: Sort out lookahead issue. Maybe by just putting production lower to set reduce/reduce conflict
--       According to http://sideshowbarker.github.com/es5-spec/#x12.4, the ambiguity is with
--       Block or FunctionDeclaration
ExpressionStatement :: { AST.JSNode }
ExpressionStatement : Expression { $1 {- ExpressionStatement -} }


-- IfStatement :                                                                            See 12.5
--         if ( Expression ) Statement else Statement
--         if ( Expression ) Statement
IfStatement :: { AST.JSNode } -- +++XXXX++
IfStatement : If LParen Expression RParen StatementSemi IfElseRest
                  { (AST.JSIf AST.JSNoAnnot $1 $2 $3 $4 $5 $6)  }

IfElseRest :: { [AST.JSNode] }
IfElseRest : Else Statement     { [$1,$2] }
           |                    { [] }

StatementSemi :: { [AST.JSNode] }
StatementSemi : StatementNoEmpty Semi { [$1,$2] {- StatementSemi1 -}}
              | StatementNoEmpty      { [$1]    {- StatementSemi2 -}}
              | Semi                  { [$1]    {- StatementSemi3 -}}


-- IterationStatement :                                                                     See 12.6
--         do Statement while ( Expression );
--         while ( Expression ) Statement
--         for (ExpressionNoInopt; Expressionopt ; Expressionopt ) Statement
--         for ( var VariableDeclarationListNoIn; Expressionopt ; Expressionopt ) Statement
--         for ( LeftHandSideExpression in Expression ) Statement
--         for ( var VariableDeclarationNoIn in Expression ) Statement
IterationStatement :: { AST.JSNode }
IterationStatement : Do Statement While LParen Expression RParen AutoSemi
                     { AST.JSDoWhile AST.JSNoAnnot $1 $2 $3 $4 $5 $6 $7 }
                   | While LParen Expression RParen Statement
                     { AST.JSWhile AST.JSNoAnnot $1 $2 $3 $4 $5 }
                   | For LParen ExpressionNoInOpt Semi ExpressionOpt Semi ExpressionOpt RParen Statement
                     { AST.JSFor AST.JSNoAnnot $1 $2 $3 $4 $5 $6 $7 $8 $9 }
                   | For LParen Var VariableDeclarationListNoIn Semi ExpressionOpt Semi ExpressionOpt RParen Statement
                     { AST.JSForVar AST.JSNoAnnot $1 $2 $3 $4 $5 $6 $7 $8 $9 $10 }
                   | For LParen LeftHandSideExpression In Expression RParen Statement
                     { AST.JSForIn AST.JSNoAnnot $1 $2 $3 $4 $5 $6 $7 }
                   | For LParen Var VariableDeclarationNoIn In Expression RParen Statement
                     { AST.JSForVarIn AST.JSNoAnnot $1 $2 $3 $4 $5 $6 $7 $8 }

-- ContinueStatement :                                                                      See 12.7
--         continue [no LineTerminator here] Identifieropt ;
-- TODO: deal with [no LineTerminator here]
ContinueStatement :: { AST.JSNode }
ContinueStatement : Continue AutoSemi             { $1 []   $2 }
                  | Continue Identifier AutoSemi  { $1 [$2] $3 }

-- BreakStatement :                                                                         See 12.8
--         break [no LineTerminator here] Identifieropt ;
-- TODO: deal with [no LineTerminator here]
BreakStatement :: { AST.JSNode }
BreakStatement : Break AutoSemi             { $1 []   $2 }
               | Break Identifier AutoSemi  { $1 [$2] $3 }

-- ReturnStatement :                                                                        See 12.9
--         return [no LineTerminator here] Expressionopt ;
-- TODO: deal with [no LineTerminator here]
ReturnStatement :: { AST.JSNode }
ReturnStatement : Return AutoSemi             { $1 []   $2 }
                | Return Expression AutoSemi  { $1 [$2] $3 }

-- WithStatement :                                                                          See 12.10
--         with ( Expression ) Statement
WithStatement :: { AST.JSNode }
WithStatement : With LParen Expression RParen Statement AutoSemi  { $1 $2 $3 $4 $5 $6 }

-- SwitchStatement :                                                                        See 12.11
--         switch ( Expression ) CaseBlock
SwitchStatement :: { AST.JSNode }
SwitchStatement : Switch LParen Expression RParen CaseBlock { $1 $2 $3 $4 $5 }

-- CaseBlock :                                                                              See 12.11
--         { CaseClausesopt }
--         { CaseClausesopt DefaultClause CaseClausesopt }
CaseBlock :: { AST.JSNode }
CaseBlock : LBrace CaseClausesOpt RBrace                              { AST.JSBlock $1 $2             $3 {- CaseBlock1 -}}
          | LBrace CaseClausesOpt DefaultClause CaseClausesOpt RBrace { AST.JSBlock $1 ($2++[$3]++$4) $5 {- CaseBlock2 -}}

-- CaseClauses :                                                                            See 12.11
--         CaseClause
--         CaseClauses CaseClause
CaseClausesOpt :: { [AST.JSNode] }
CaseClausesOpt : CaseClause                { [$1] {- CaseClauses1 -}}
               | CaseClausesOpt CaseClause { ($1++[$2]) {- CaseClauses2 -}}
               |                           { [AST.JSLiteral AST.JSNoAnnot ""] } -- { [] }

-- CaseClause :                                                               See 12.11
--        case Expression : StatementListopt
CaseClause :: { AST.JSNode }
CaseClause : Case Expression Colon StatementList  { $1 $2 $3 $4 }
           | Case Expression Colon                { $1 $2 $3 [] }

-- DefaultClause :                                                            See 12.11
--        default : StatementListopt
DefaultClause :: { AST.JSNode }
DefaultClause : Default Colon                { $1 $2 [] }
              | Default Colon StatementList  { $1 $2 $3 }

-- LabelledStatement :                                                        See 12.12
--        Identifier : Statement
LabelledStatement :: { AST.JSNode }
LabelledStatement : Identifier Colon Statement { AST.JSLabelled AST.JSNoAnnot $1 $2 $3 }

-- ThrowStatement :                                                           See 12.13
--        throw [no LineTerminator here] Expression ;
-- TODO : sort out no LineTerminator here
--        Does it need a semi at the end?
ThrowStatement :: { AST.JSNode }
ThrowStatement : Throw Expression { $1 $2 }

-- Note: worked in updated syntax as per https://developer.mozilla.org/en/JavaScript/Reference/Statements/try...catch
--   i.e., 0 or more catches, then an optional finally
-- TryStatement :                                                             See 12.14
--        try Block Catch
--        try Block Finally
--        try Block Catch Finally
TryStatement :: { AST.JSNode }
TryStatement : Try Block Catches         { $1 $2 $3         {- TryStatement1 -} }
             | Try Block Finally         { $1 $2 [$3]       {- TryStatement2 -} }
             | Try Block Catches Finally { $1 $2 ($3++[$4]) {- TryStatement3 -} }

Catches :: { [AST.JSNode] }
Catches : Catch         { [$1]       {- Catches 1 -} }
        | Catches Catch { ($1++[$2]) {- Catches 2 -} }

-- Note: worked in updated syntax as per https://developer.mozilla.org/en/JavaScript/Reference/Statements/try...catch
-- <Catch> ::= 'catch' '(' Identifier ')' <Block>
--   becomes
-- <Catch> ::= 'catch' '(' Identifier ')' <Block>
--           | 'catch' '(' Identifier 'if' ConditionalExpression ')' <Block>
Catch :: { AST.JSNode }
Catch : CatchL LParen Identifier                          RParen Block { $1 $2 $3 [     ] $4 $5 }
      | CatchL LParen Identifier If ConditionalExpression RParen Block { $1 $2 $3 ($4:$5) $6 $7 }

-- Finally :                                                                  See 12.14
--        finally Block
Finally :: { AST.JSNode }
Finally : FinallyL Block { $1 $2 }

-- DebuggerStatement :                                                        See 12.15
--        debugger ;
DebuggerStatement :: { AST.JSNode }
DebuggerStatement : 'debugger' AutoSemi { AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "debugger" }

-- FunctionDeclaration :                                                      See clause 13
--        function Identifier ( FormalParameterListopt ) { FunctionBody }
FunctionDeclaration :: { AST.JSNode }
FunctionDeclaration : Function Identifier LParen FormalParameterList RParen FunctionBody
                      { AST.JSFunction AST.JSNoAnnot $1 $2 $3 $4 $5 $6 }
                    | Function Identifier LParen RParen FunctionBody
                      { AST.JSFunction AST.JSNoAnnot $1 $2 $3 [] $4 $5 }

-- FunctionExpression :                                                       See clause 13
--        function Identifieropt ( FormalParameterListopt ) { FunctionBody }
FunctionExpression :: { AST.JSNode }
FunctionExpression : Function IdentifierOpt LParen RParen FunctionBody
                     { AST.JSFunctionExpression AST.JSNoAnnot $1 $2 $3 [] $4 $5 }
                   | Function IdentifierOpt LParen FormalParameterList RParen FunctionBody
                     { AST.JSFunctionExpression AST.JSNoAnnot $1 $2 $3 $4 $5 $6  }

IdentifierOpt :: { [AST.JSNode] }
IdentifierOpt : Identifier { [$1] {- IdentifierOpt -}}
              |            { []   {- IdentifierOpt -}}

-- FormalParameterList :                                                      See clause 13
--        Identifier
--        FormalParameterList , Identifier
FormalParameterList :: { [AST.JSNode] }
FormalParameterList : Identifier                            { [$1] {- FormalParameterList -}}
                    | FormalParameterList Comma Identifier  { ($1++[$2]++[$3]) }

-- FunctionBody :                                                             See clause 13
--        SourceElementsopt
FunctionBody :: { AST.JSNode }
FunctionBody : LBrace SourceElements RBrace { AST.JSBlock $1 $2 $3 }
             | LBrace                RBrace { AST.JSBlock $1 [] $2 }

-- Program :                                                                  See clause 14
--        SourceElementsopt

Program :: { AST.JSNode }
Program : SourceElementsTop Eof { (combineTop $1 $2) {- Program -}}
        | Eof                   { AST.JSSourceElementsTop AST.JSNoAnnot [$1] }

-- For debugging/other entry points
LiteralMain :: { AST.JSNode }
LiteralMain : Literal Eof { $1 }

PrimaryExpressionMain :: { AST.JSNode }
PrimaryExpressionMain : PrimaryExpression Eof { $1 }

StatementMain :: { AST.JSNode }
StatementMain : Statement Eof { $1 }


-- SourceElements :                                                           See clause 14
--        SourceElement
--        SourceElements SourceElement
SourceElements :: { [AST.JSNode] }
SourceElements : SourceElement                { [$1]     {- SourceElements -} }
               | SourceElements SourceElement { $1++[$2] {- SourceElements -} }

SourceElementsTop :: { AST.JSNode }
SourceElementsTop : SourceElement                   { AST.JSSourceElementsTop AST.JSNoAnnot [$1] }
                  | SourceElementsTop SourceElement { (combineSourceElementsTop $1 $2) }

-- SourceElement :
--       Statement
--       FunctionDeclaration
SourceElement :: { AST.JSNode }
SourceElement : Statement            { $1 {- SourceElement1 -} }
              | FunctionDeclaration  { $1 {- SourceElement2 -} }

{
combineSourceElementsTop :: AST.JSNode -> AST.JSNode -> AST.JSNode
combineSourceElementsTop (AST.JSSourceElementsTop AST.JSNoAnnot xs) x1 = AST.JSSourceElementsTop AST.JSNoAnnot (xs++[x1])

combineTop :: AST.JSNode -> AST.JSNode -> AST.JSNode
combineTop (AST.JSSourceElementsTop AST.JSNoAnnot xs) x1 = AST.JSSourceElementsTop AST.JSNoAnnot (xs++[x1])


parseError :: Token -> Alex a
parseError tok = alexError (show tok)

-- --------------------------------

ss :: Token -> TokenPosn
ss token = token_span token

-- ------------------------------

gc :: Token -> [CommentAnnotation]
gc token = token_comment token
mgc :: [Token] -> [CommentAnnotation]
mgc xs = concatMap gc xs

-- ---------------------------------------------------------------------
-- | mkUnary : The parser detects '+' and '-' as the binary version of these
-- operator. This function converts from the binary version to the unary
-- version.
mkUnary :: AST.JSBinOp -> AST.JSUnaryOp
mkUnary (AST.JSBinOpMinus annot) = AST.JSUnaryOpMinus annot
mkUnary (AST.JSBinOpPlus  annot) = AST.JSUnaryOpPlus  annot

mkUnary x = error $ "Invalid unary op : " ++ show x

}

-- Set emacs mode
-- Local Variables:
-- mode:haskell
-- End:
