{
{-# LANGUAGE BangPatterns #-}
module Language.JavaScript.Parser.Grammar5 (
    parseProgram
  , parseLiteral
  , parsePrimaryExpression
  , parseStatement
  ) where

import Data.Char
import Language.JavaScript.Parser.Lexer
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
     '*='	{ TimesAssignToken {} }
     '/='	{ DivideAssignToken {} }
     '%='	{ ModAssignToken {} }
     '+='	{ PlusAssignToken {} }
     '-='	{ MinusAssignToken {} }
     '<<='	{ LshAssignToken {} }
     '>>='	{ RshAssignToken {} }
     '>>>='	{ UrshAssignToken {} }
     '&='	{ AndAssignToken {} }
     '^='	{ XorAssignToken {} }
     '|='	{ OrAssignToken {} }
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

LParen :: { AST.JSAnnot }
LParen : '(' { AST.JSAnnot (ss $1) (gc $1) }

RParen :: { AST.JSAnnot }
RParen : ')' { AST.JSAnnot (ss $1) (gc $1) }

LBrace :: { AST.JSAnnot }
LBrace : '{' { AST.JSAnnot (ss $1) (gc $1) }

RBrace :: { AST.JSAnnot }
RBrace : '}' { AST.JSAnnot (ss $1) (gc $1) }

LSquare :: { AST.JSAnnot }
LSquare : '[' { AST.JSAnnot (ss $1) (gc $1) }

RSquare :: { AST.JSAnnot }
RSquare : ']' { AST.JSAnnot (ss $1) (gc $1) }

Comma :: { AST.JSExpression }
Comma : ',' { AST.JSComma (AST.JSAnnot (ss $1) (gc $1)) }

Colon :: { AST.JSAnnot }
Colon : ':' { AST.JSAnnot (ss $1) (gc $1) }

Semi :: { AST.JSAnnot }
Semi : ';' { AST.JSAnnot (ss $1) (gc $1) }

Dot :: { AST.JSAnnot }
Dot : '.' { AST.JSAnnot (ss $1) (gc $1) }

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

Hook :: { AST.JSAnnot }
Hook : '?' { AST.JSAnnot (ss $1) (gc $1) }

SimpleAssign :: { AST.JSAnnot }
SimpleAssign : '=' { AST.JSAnnot (ss $1) (gc $1) }

OpAssign :: { AST.JSAssignOp }
OpAssign : '*='	  { AST.JSTimesAssign  (AST.JSAnnot (ss $1) (gc $1)) }
         | '/='	  { AST.JSDivideAssign (AST.JSAnnot (ss $1) (gc $1)) }
         | '%='	  { AST.JSModAssign    (AST.JSAnnot (ss $1) (gc $1)) }
         | '+='	  { AST.JSPlusAssign   (AST.JSAnnot (ss $1) (gc $1)) }
         | '-='	  { AST.JSMinusAssign  (AST.JSAnnot (ss $1) (gc $1)) }
         | '<<='  { AST.JSLshAssign    (AST.JSAnnot (ss $1) (gc $1)) }
         | '>>='  { AST.JSRshAssign    (AST.JSAnnot (ss $1) (gc $1)) }
         | '>>>=' { AST.JSUrshAssign   (AST.JSAnnot (ss $1) (gc $1)) }
         | '&='	  { AST.JSBwAndAssign  (AST.JSAnnot (ss $1) (gc $1)) }
         | '^='	  { AST.JSBwXorAssign  (AST.JSAnnot (ss $1) (gc $1)) }
         | '|='	  { AST.JSBwOrAssign   (AST.JSAnnot (ss $1) (gc $1)) }

Assign :: { AST.JSExpression }
Assign : 'assign' { AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) (tokenLiteral $1) }

Var :: { AST.JSAnnot }
Var : 'var' { AST.JSAnnot (ss $1) (gc $1) }

Const :: { AST.JSAnnot }
Const : 'const' { AST.JSAnnot (ss $1) (gc $1) }

If :: { AST.JSAnnot }
If : 'if' { AST.JSAnnot (ss $1) (gc $1) }

Else :: { AST.JSAnnot }
Else : 'else' { AST.JSAnnot (ss $1) (gc $1) }

Do :: { AST.JSAnnot }
Do : 'do' { AST.JSAnnot (ss $1) (gc $1) }

While :: { AST.JSAnnot }
While : 'while' { AST.JSAnnot (ss $1) (gc $1) }

For :: { AST.JSAnnot }
For : 'for' { AST.JSAnnot (ss $1) (gc $1) }

Continue :: { AST.JSAnnot }
Continue : 'continue' { AST.JSAnnot (ss $1) (gc $1) }

Break :: { AST.JSAnnot }
Break : 'break' { AST.JSAnnot (ss $1) (gc $1) }

Return :: { AST.JSAnnot }
Return : 'return' { AST.JSAnnot (ss $1) (gc $1) }

With :: { AST.JSAnnot }
With : 'with' { AST.JSAnnot (ss $1) (gc $1) }

Switch :: { AST.JSAnnot }
Switch : 'switch' { AST.JSAnnot (ss $1) (gc $1) }

Case :: { AST.JSAnnot }
Case : 'case' { AST.JSAnnot (ss $1) (gc $1) }

Default :: { AST.JSAnnot }
Default : 'default' { AST.JSAnnot (ss $1) (gc $1) }

Throw :: { AST.JSAnnot }
Throw : 'throw' { AST.JSAnnot (ss $1) (gc $1) {- 'Throw' -} }

Try :: { AST.JSAnnot }
Try : 'try' { AST.JSAnnot (ss $1) (gc $1) }

CatchL :: { AST.JSAnnot }
CatchL : 'catch' { AST.JSAnnot (ss $1) (gc $1) }

FinallyL :: { AST.JSAnnot }
FinallyL : 'finally' { AST.JSAnnot (ss $1) (gc $1) }

Function :: { AST.JSAnnot }
Function : 'function' { AST.JSAnnot (ss $1) (gc $1) {- 'Function' -} }

New :: { AST.JSAnnot }
New : 'new' { AST.JSAnnot (ss $1) (gc $1) }


Eof :: { AST.JSStatement }
Eof : 'tail' { AST.JSExpressionStatement (AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "") AST.JSSemiAuto {- 'Eof' -} }

-- Literal ::                                                                See 7.8
--         NullLiteral
--         BooleanLiteral
--         NumericLiteral
--         StringLiteral
Literal :: { AST.JSExpression }
Literal : NullLiteral     { $1 }
        | BooleanLiteral  { $1 }
        | NumericLiteral  { $1 }
        | StringLiteral   { $1 }
        | RegularExpressionLiteral { $1 }

NullLiteral :: { AST.JSExpression }
NullLiteral : 'null' { AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "null" }

BooleanLiteral :: { AST.JSExpression }
BooleanLiteral : 'true'  { AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "true" }
               | 'false' { AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "false" }

-- <Numeric Literal> ::= DecimalLiteral
--                     | HexIntegerLiteral
--                     | OctalLiteral
NumericLiteral :: { AST.JSExpression }
NumericLiteral : 'decimal'    { AST.JSDecimal (AST.JSAnnot (ss $1) (gc $1)) (tokenLiteral $1) }
               | 'hexinteger' { AST.JSHexInteger (AST.JSAnnot (ss $1) (gc $1)) (tokenLiteral $1) }
               | 'octal'      { AST.JSOctal (AST.JSAnnot (ss $1) (gc $1)) (tokenLiteral $1) }

StringLiteral :: { AST.JSExpression }
StringLiteral : 'string'  { case (token_delimiter $1) of
								'\'' -> AST.JSStringLiteralS (AST.JSAnnot (ss $1) (gc $1)) (tokenLiteral $1)
								'\"' -> AST.JSStringLiteralD (AST.JSAnnot (ss $1) (gc $1)) (tokenLiteral $1)
								}

-- <Regular Expression Literal> ::= RegExp
RegularExpressionLiteral :: { AST.JSExpression }
RegularExpressionLiteral : 'regex' { AST.JSRegEx (AST.JSAnnot (ss $1) (gc $1)) (tokenLiteral $1) }

-- PrimaryExpression :                                                   See 11.1
--        this
--        Identifier
--        Literal
--        ArrayLiteral
--        ObjectLiteral
--        ( Expression )
PrimaryExpression :: { AST.JSExpression }
PrimaryExpression : 'this'                   { AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "this" }
                  | Identifier               { $1 {- 'PrimaryExpression1' -} }
                  | Literal                  { $1 {- 'PrimaryExpression2' -} }
                  | ArrayLiteral             { $1 {- 'PrimaryExpression3' -} }
                  | ObjectLiteral            { $1 {- 'PrimaryExpression4' -} }
                  | LParen Expression RParen { AST.JSExpressionParen $1 $2 $3 }

-- Identifier ::                                                            See 7.6
--         IdentifierName but not ReservedWord
-- IdentifierName ::                                                        See 7.6
--         IdentifierStart
--         IdentifierName IdentifierPart
Identifier :: { AST.JSExpression }
Identifier : 'ident' { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) (tokenLiteral $1) }
           | 'get'   { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "get" }
           | 'set'   { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) "set" }

-- TODO: make this include any reserved word too, including future ones
IdentifierName :: { AST.JSExpression }
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
             | 'future'     { AST.JSIdentifier (AST.JSAnnot (ss $1) (gc $1)) (tokenLiteral $1) }



-- ArrayLiteral :                                                        See 11.1.4
--        [ Elisionopt ]
--        [ ElementList ]
--        [ ElementList , Elisionopt ]
ArrayLiteral :: { AST.JSExpression }
ArrayLiteral : LSquare RSquare                           { AST.JSArrayLiteral $1 [] $2 }
             | LSquare Elision RSquare                   { AST.JSArrayLiteral $1 $2 $3 }
             | LSquare ElementList RSquare               { AST.JSArrayLiteral $1 $2 $3 }
             | LSquare ElementList Elision RSquare       { AST.JSArrayLiteral $1 ($2 ++ $3) $4 }


-- ElementList :                                                         See 11.1.4
--        Elisionopt AssignmentExpression
--        ElementList , Elisionopt AssignmentExpression
ElementList :: { [AST.JSExpression] }
ElementList : Elision AssignmentExpression                   { $1 ++ [$2]   {- 'ElementList1' -} }
            | AssignmentExpression                           { [$1]         {- 'ElementList2' -} }
            | ElementList Elision AssignmentExpression       { (($1)++($2 ++ [$3])) {- 'ElementList3' -} }


-- Elision :                                                             See 11.1.4
--        ,
--        Elision ,
Elision :: { [AST.JSExpression] }
Elision : Comma         { [$1] }
        | Comma Elision { $1:$2 }

-- ObjectLiteral :                                                       See 11.1.5
--        { }
--        { PropertyNameAndValueList }
--        { PropertyNameAndValueList , }
ObjectLiteral :: { AST.JSExpression }
ObjectLiteral : LBrace RBrace                                { AST.JSObjectLiteral $1 [] $2          }
              | LBrace PropertyNameandValueList RBrace       { AST.JSObjectLiteral $1 $2 $3          }
              | LBrace PropertyNameandValueList Comma RBrace { AST.JSObjectLiteral $1 ($2++[$3]) $4  }

-- <Property Name and Value List> ::= <Property Name> ':' <Assignment Expression>
--                                  | <Property Name and Value List> ',' <Property Name> ':' <Assignment Expression>

-- Seems we can have function declarations in the value part too
-- PropertyNameAndValueList :                                            See 11.1.5
--        PropertyAssignment
--        PropertyNameAndValueList , PropertyAssignment
PropertyNameandValueList :: { [ AST.JSExpression ] }
PropertyNameandValueList : PropertyAssignment                              { [$1] {- 'PropertyNameandValueList1' -} }
                         | PropertyNameandValueList Comma PropertyAssignment { ($1++[$2]++[$3]) {- 'PropertyNameandValueList2' -} }

-- PropertyAssignment :                                                  See 11.1.5
--        PropertyName : AssignmentExpression
--        get PropertyName() { FunctionBody }
--        set PropertyName( PropertySetParameterList ) { FunctionBody }
-- TODO: not clear if get/set are keywords, or just used in a specific context. Puzzling.
PropertyAssignment :: { AST.JSExpression }
PropertyAssignment : PropertyName Colon AssignmentExpression { AST.JSPropertyNameandValue (identName $1) $2 [$3] }
                   -- Should be "get" in next, but is not a Token
                   | 'get' PropertyName LParen RParen FunctionBody
                       { AST.JSPropertyAccessor (AST.JSAccessorGet (AST.JSAnnot (ss $1) (gc $1))) (identName $2) $3 [] $4 $5 }
                   -- Should be "set" in next, but is not a Token
                   | 'set' PropertyName LParen PropertySetParameterList RParen FunctionBody
                       { AST.JSPropertyAccessor (AST.JSAccessorSet (AST.JSAnnot (ss $1) (gc $1))) (identName $2) $3 [$4] $5 $6 }

-- PropertyName :                                                        See 11.1.5
--        IdentifierName
--        StringLiteral
--        NumericLiteral
PropertyName :: { AST.JSExpression }
PropertyName : IdentifierName { $1 {- 'PropertyName1' -} }
             | StringLiteral  { $1 {- 'PropertyName2' -} }
             | NumericLiteral { $1 {- 'PropertyName3' -} }

-- PropertySetParameterList :                                            See 11.1.5
--        Identifier
PropertySetParameterList :: { AST.JSExpression }
PropertySetParameterList : Identifier { $1 {- 'PropertySetParameterList' -} }

-- MemberExpression :                                           See 11.2
--        PrimaryExpression
--        FunctionExpression
--        MemberExpression [ Expression ]
--        MemberExpression . IdentifierName
--        new MemberExpression Arguments
MemberExpression :: { AST.JSExpression }
MemberExpression : PrimaryExpression   { $1 {- 'MemberExpression1' -} }
                 | FunctionExpression  { $1 {- 'MemberExpression2' -} }
                 | MemberExpression LSquare Expression RSquare { AST.JSMemberSquare $1 $2 $3 $4 {- 'MemberExpression3' -} }
                 | MemberExpression Dot IdentifierName         { AST.JSMemberDot $1 $2 $3       {- 'MemberExpression4' -} }
                 | New MemberExpression Arguments              { AST.JSMemberNew $1 $2 $3       {- 'MemberExpression5' -} }

-- NewExpression :                                              See 11.2
--        MemberExpression
--        new NewExpression
NewExpression :: { AST.JSExpression }
NewExpression : MemberExpression    { $1                        {- 'NewExpression1' -} }
              | New NewExpression   { AST.JSNewExpression $1 $2 {- 'NewExpression2' -} }

-- CallExpression :                                             See 11.2
--        MemberExpression Arguments
--        CallExpression Arguments
--        CallExpression [ Expression ]
--        CallExpression . IdentifierName
CallExpression :: { AST.JSExpression }
CallExpression : MemberExpression Arguments
                    { AST.JSMemberExpression $1 $2 {- 'CallExpression1' -} }
               | CallExpression Arguments
                    { AST.JSCallExpression $1 $2 {- 'CallExpression2' -} }
               | CallExpression LSquare Expression RSquare
                    { AST.JSCallExpressionSquare $1 $2 $3 $4 {- 'CallExpression3' -} }
               | CallExpression Dot IdentifierName
                    { AST.JSCallExpressionDot $1 $2 $3 {- 'CallExpression4' -} }

-- Arguments :                                                  See 11.2
--        ()
--        ( ArgumentList )
Arguments :: { AST.JSArguments }
Arguments : LParen RParen               { AST.JSArguments $1 AST.JSNoParams $2    {- 'Arguments1' -} }
          | LParen ArgumentList RParen  { AST.JSArguments $1 (AST.JSParams $2) $3 {- 'Arguments2' -} }

-- ArgumentList :                                               See 11.2
--        AssignmentExpression
--        ArgumentList , AssignmentExpression
ArgumentList :: { AST.JSNonEmptyList AST.JSExpression }
ArgumentList : AssignmentExpression                    { AST.JSLOne $1                  {- 'ArgumentList1' -} }
             | ArgumentList Comma AssignmentExpression { AST.JSLCons $1 (nodePos $2) $3 {- 'ArgumentList2' -} }

-- LeftHandSideExpression :                                     See 11.2
--        NewExpression
--        CallExpression
LeftHandSideExpression :: { AST.JSExpression }
LeftHandSideExpression : NewExpression  { $1 {- 'LeftHandSideExpression1' -} }
                       | CallExpression { $1 {- 'LeftHandSideExpression12' -} }

-- PostfixExpression :                                          See 11.3
--        LeftHandSideExpression
--                                  [no LineTerminator here]
--        LeftHandSideExpression                             ++
--                                  [no LineTerminator here]
--        LeftHandSideExpression                             --
PostfixExpression :: { AST.JSExpression }
PostfixExpression : LeftHandSideExpression { $1 {- 'PostfixExpression' -} }
                  | PostfixExpression Increment { AST.JSExpressionPostfix $1 $2 }
                  | PostfixExpression Decrement { AST.JSExpressionPostfix $1 $2 }

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
UnaryExpression :: { AST.JSExpression }
UnaryExpression : PostfixExpression { $1 {- 'UnaryExpression' -} }
                | Delete    UnaryExpression { AST.JSUnaryExpression $1 $2 }
                | Void      UnaryExpression { AST.JSUnaryExpression $1 $2 }
                | Typeof    UnaryExpression { AST.JSUnaryExpression $1 $2 }
                | Increment UnaryExpression { AST.JSUnaryExpression $1 $2 }
                | Decrement UnaryExpression { AST.JSUnaryExpression $1 $2 }
                | Plus      UnaryExpression { AST.JSUnaryExpression (mkUnary $1) $2 }
                | Minus     UnaryExpression { AST.JSUnaryExpression (mkUnary $1) $2 }
                | Tilde     UnaryExpression { AST.JSUnaryExpression $1 $2 }
                | Not       UnaryExpression { AST.JSUnaryExpression $1 $2 }

-- MultiplicativeExpression :                                   See 11.5
--        UnaryExpression
--        MultiplicativeExpression * UnaryExpression
--        MultiplicativeExpression / UnaryExpression
--        MultiplicativeExpression % UnaryExpression
MultiplicativeExpression :: { AST.JSExpression }
MultiplicativeExpression : UnaryExpression { $1 {- 'MultiplicativeExpression' -} }
                         | MultiplicativeExpression Mul UnaryExpression { AST.JSExpressionBinary {- '*' -} $1 $2 $3 }
                         | MultiplicativeExpression Div UnaryExpression { AST.JSExpressionBinary {- '/' -} $1 $2 $3 }
                         | MultiplicativeExpression Mod UnaryExpression { AST.JSExpressionBinary {- '%' -} $1 $2 $3 }

-- AdditiveExpression :                                        See 11.6
--        MultiplicativeExpression
--        AdditiveExpression + MultiplicativeExpression
--        AdditiveExpression - MultiplicativeExpression
AdditiveExpression :: { AST.JSExpression }
AdditiveExpression : AdditiveExpression Plus  MultiplicativeExpression { AST.JSExpressionBinary {- '+' -} $1 $2 $3 }
                   | AdditiveExpression Minus MultiplicativeExpression { AST.JSExpressionBinary {- '-' -} $1 $2 $3 }
                   | MultiplicativeExpression { $1 {- '(goRegExp $1)-} {- 'AdditiveExpression' -} }

-- ShiftExpression :                                           See 11.7
--        AdditiveExpression
--        ShiftExpression << AdditiveExpression
--        ShiftExpression >> AdditiveExpression
--        ShiftExpression >>> AdditiveExpression
ShiftExpression :: { AST.JSExpression }
ShiftExpression : ShiftExpression Lsh  AdditiveExpression { AST.JSExpressionBinary {- '<<' -}  $1 $2 $3 }
                | ShiftExpression Rsh  AdditiveExpression { AST.JSExpressionBinary {- '>>' -}  $1 $2 $3 }
                | ShiftExpression Ursh AdditiveExpression { AST.JSExpressionBinary {- '>>>' -} $1 $2 $3 }
                | AdditiveExpression { $1 {- 'ShiftExpression' -} }

-- RelationalExpression :                                      See 11.8
--        ShiftExpression
--        RelationalExpression < ShiftExpression
--        RelationalExpression > ShiftExpression
--        RelationalExpression <= ShiftExpression
--        RelationalExpression >= ShiftExpression
--        RelationalExpression instanceof ShiftExpression
--        RelationalExpression in ShiftExpression
RelationalExpression :: { AST.JSExpression }
RelationalExpression : ShiftExpression { $1 {- 'RelationalExpression' -} }
                     | RelationalExpression Lt  ShiftExpression { AST.JSExpressionBinary {- '<' -}  $1 $2 $3 }
                     | RelationalExpression Gt  ShiftExpression { AST.JSExpressionBinary {- '>' -}  $1 $2 $3 }
                     | RelationalExpression Le  ShiftExpression { AST.JSExpressionBinary {- '<=' -} $1 $2 $3 }
                     | RelationalExpression Ge  ShiftExpression { AST.JSExpressionBinary {- '>=' -} $1 $2 $3 }
                     | RelationalExpression Instanceof ShiftExpression { AST.JSExpressionBinary {- ' instanceof' -} $1 $2 $3 }
                     | RelationalExpression In         ShiftExpression { AST.JSExpressionBinary {- ' in        ' -} $1 $2 $3 }

-- RelationalExpressionNoIn :                                  See 11.8
--        ShiftExpression
--        RelationalExpressionNoIn < ShiftExpression
--        RelationalExpressionNoIn > ShiftExpression
--        RelationalExpressionNoIn <= ShiftExpression
--        RelationalExpressionNoIn >= ShiftExpression
--        RelationalExpressionNoIn instanceof ShiftExpression
RelationalExpressionNoIn :: { AST.JSExpression }
RelationalExpressionNoIn : ShiftExpression { $1 {- 'RelationalExpressionNoIn' -} }
                     | RelationalExpressionNoIn Lt  ShiftExpression { AST.JSExpressionBinary {- '<' -}  $1 $2 $3 }
                     | RelationalExpressionNoIn Gt  ShiftExpression { AST.JSExpressionBinary {- '>' -}  $1 $2 $3 }
                     | RelationalExpressionNoIn Le  ShiftExpression { AST.JSExpressionBinary {- '<=' -} $1 $2 $3 }
                     | RelationalExpressionNoIn Ge  ShiftExpression { AST.JSExpressionBinary {- '>=' -} $1 $2 $3 }
                     | RelationalExpressionNoIn Instanceof ShiftExpression { AST.JSExpressionBinary {- ' instanceof ' -} $1 $2 $3 }

-- EqualityExpression :                                        See 11.9
--        RelationalExpression
--        EqualityExpression == RelationalExpression
--        EqualityExpression != RelationalExpression
--        EqualityExpression === RelationalExpression
--        EqualityExpression !== RelationalExpression
EqualityExpression :: { AST.JSExpression }
EqualityExpression : RelationalExpression { $1 {- 'EqualityExpression' -} }
                   | EqualityExpression Equal    RelationalExpression { AST.JSExpressionBinary {- '==' -}  $1 $2 $3 }
                   | EqualityExpression Ne       RelationalExpression { AST.JSExpressionBinary {- '!=' -}  $1 $2 $3 }
                   | EqualityExpression StrictEq RelationalExpression { AST.JSExpressionBinary {- '===' -} $1 $2 $3 }
                   | EqualityExpression StrictNe RelationalExpression { AST.JSExpressionBinary {- '!==' -} $1 $2 $3 }

-- EqualityExpressionNoIn :                                    See 11.9
--        RelationalExpressionNoIn
--        EqualityExpressionNoIn == RelationalExpressionNoIn
--        EqualityExpressionNoIn != RelationalExpressionNoIn
--        EqualityExpressionNoIn === RelationalExpressionNoIn
--        EqualityExpressionNoIn !== RelationalExpressionNoIn
EqualityExpressionNoIn :: { AST.JSExpression }
EqualityExpressionNoIn : RelationalExpressionNoIn { $1 {- 'EqualityExpressionNoIn' -} }
                       | EqualityExpressionNoIn Equal    RelationalExpression { AST.JSExpressionBinary {- '==' -}  $1 $2 $3 }
                       | EqualityExpressionNoIn Ne       RelationalExpression { AST.JSExpressionBinary {- '!=' -}  $1 $2 $3 }
                       | EqualityExpressionNoIn StrictEq RelationalExpression { AST.JSExpressionBinary {- '===' -} $1 $2 $3 }
                       | EqualityExpressionNoIn StrictNe RelationalExpression { AST.JSExpressionBinary {- '!==' -} $1 $2 $3 }

-- BitwiseANDExpression :                                      See 11.10
--        EqualityExpression
--        BitwiseANDExpression & EqualityExpression
BitwiseAndExpression :: { AST.JSExpression }
BitwiseAndExpression : EqualityExpression { $1 {- 'BitwiseAndExpression' -} }
                     | BitwiseAndExpression BitAnd EqualityExpression { AST.JSExpressionBinary {- '&' -} $1 $2 $3 }

-- BitwiseANDExpressionNoIn :                                  See 11.10
--        EqualityExpressionNoIn
--        BitwiseANDExpressionNoIn & EqualityExpressionNoIn
BitwiseAndExpressionNoIn :: { AST.JSExpression }
BitwiseAndExpressionNoIn : EqualityExpressionNoIn { $1 {- 'BitwiseAndExpression' -} }
                     | BitwiseAndExpressionNoIn BitAnd EqualityExpressionNoIn { AST.JSExpressionBinary {- '&' -} $1 $2 $3 }

-- BitwiseXORExpression :                                                                See 11.10
--        BitwiseANDExpression
--        BitwiseXORExpression ^ BitwiseANDExpression
BitwiseXOrExpression :: { AST.JSExpression }
BitwiseXOrExpression : BitwiseAndExpression { $1 {- 'BitwiseXOrExpression' -} }
                     | BitwiseXOrExpression BitXor BitwiseAndExpression { AST.JSExpressionBinary {- '^' -} $1 $2 $3 }

-- BitwiseXORExpressionNoIn :                                                            See 11.10
--        BitwiseANDExpressionNoIn
--        BitwiseXORExpressionNoIn ^ BitwiseANDExpressionNoIn
BitwiseXOrExpressionNoIn :: { AST.JSExpression }
BitwiseXOrExpressionNoIn : BitwiseAndExpressionNoIn { $1 {- 'BitwiseXOrExpression' -} }
                         | BitwiseXOrExpressionNoIn BitXor BitwiseAndExpressionNoIn { AST.JSExpressionBinary {- '^' -} $1 $2 $3 }

-- BitwiseORExpression :                                                                 See 11.10
--        BitwiseXORExpression
--        BitwiseORExpression | BitwiseXORExpression
BitwiseOrExpression :: { AST.JSExpression }
BitwiseOrExpression : BitwiseXOrExpression { $1 {- 'BitwiseOrExpression' -} }
                    | BitwiseOrExpression BitOr BitwiseXOrExpression { AST.JSExpressionBinary {- '|' -} $1 $2 $3 }

-- BitwiseORExpressionNoIn :                                                             See 11.10
--        BitwiseXORExpressionNoIn
--        BitwiseORExpressionNoIn | BitwiseXORExpressionNoIn
BitwiseOrExpressionNoIn :: { AST.JSExpression }
BitwiseOrExpressionNoIn : BitwiseXOrExpressionNoIn { $1 {- 'BitwiseOrExpression' -} }
                        | BitwiseOrExpressionNoIn BitOr BitwiseXOrExpressionNoIn { AST.JSExpressionBinary {- '|' -} $1 $2 $3 }

-- LogicalANDExpression :                                                                See 11.11
--        BitwiseORExpression
--        LogicalANDExpression && BitwiseORExpression
LogicalAndExpression :: { AST.JSExpression }
LogicalAndExpression : BitwiseOrExpression { $1 {- 'LogicalAndExpression' -} }
                     | LogicalAndExpression And BitwiseOrExpression { AST.JSExpressionBinary {- '&&' -} $1 $2 $3 }

-- LogicalANDExpressionNoIn :                                                            See 11.11
--        BitwiseORExpressionNoIn
--        LogicalANDExpressionNoIn && BitwiseORExpressionNoIn
LogicalAndExpressionNoIn :: { AST.JSExpression }
LogicalAndExpressionNoIn : BitwiseOrExpressionNoIn { $1 {- 'LogicalAndExpression' -} }
                         | LogicalAndExpressionNoIn And BitwiseOrExpressionNoIn { AST.JSExpressionBinary {- '&&' -} $1 $2 $3 }

-- LogicalORExpression :                                                                 See 11.11
--        LogicalANDExpression
--        LogicalORExpression || LogicalANDExpression
LogicalOrExpression :: { AST.JSExpression }
LogicalOrExpression : LogicalAndExpression { $1 {- 'LogicalOrExpression' -} }
                    | LogicalOrExpression Or LogicalAndExpression { AST.JSExpressionBinary {- '||' -} $1 $2 $3 }

-- LogicalORExpressionNoIn :                                                             See 11.11
--        LogicalANDExpressionNoIn
--        LogicalORExpressionNoIn || LogicalANDExpressionNoIn
LogicalOrExpressionNoIn :: { AST.JSExpression }
LogicalOrExpressionNoIn : LogicalAndExpressionNoIn { $1 {- 'LogicalOrExpression' -} }
                        | LogicalOrExpressionNoIn Or LogicalAndExpressionNoIn { AST.JSExpressionBinary {- '||' -} $1 $2 $3 }

-- ConditionalExpression :                                                               See 11.12
--        LogicalORExpression
--        LogicalORExpression ? AssignmentExpression : AssignmentExpression
ConditionalExpression :: { AST.JSExpression }
ConditionalExpression : LogicalOrExpression { $1 {- 'ConditionalExpression1' -} }
                      | LogicalOrExpression Hook AssignmentExpression Colon AssignmentExpression
                        { AST.JSExpressionTernary $1 $2 $3 $4 $5 {- 'ConditionalExpression2' -}  }

-- ConditionalExpressionNoIn :                                                           See 11.12
--        LogicalORExpressionNoIn
--        LogicalORExpressionNoIn ? AssignmentExpressionNoIn : AssignmentExpressionNoIn
ConditionalExpressionNoIn :: { AST.JSExpression }
ConditionalExpressionNoIn : LogicalOrExpressionNoIn { $1 {- 'ConditionalExpressionNoIn1' -} }
                          | LogicalOrExpressionNoIn Hook AssignmentExpressionNoIn Colon AssignmentExpressionNoIn
                            { AST.JSExpressionTernary $1 $2 $3 $4 $5 {- 'ConditionalExpressionNoIn2' -} }

-- AssignmentExpression :                                                                See 11.13
--        ConditionalExpression
--        LeftHandSideExpression AssignmentOperator AssignmentExpression
AssignmentExpression :: { AST.JSExpression }
AssignmentExpression : ConditionalExpression { $1 {- 'AssignmentExpression1' -} }
                     | LeftHandSideExpression AssignmentOperator AssignmentExpression
                       { AST.JSAssignExpression $1 $2 $3 {- 'AssignmentExpression2' -} }

-- AssignmentExpressionNoIn :                                                            See 11.13
--        ConditionalExpressionNoIn
--        LeftHandSideExpression AssignmentOperator AssignmentExpressionNoIn
AssignmentExpressionNoIn :: { AST.JSExpression }
AssignmentExpressionNoIn : ConditionalExpressionNoIn { $1 {- 'AssignmentExpressionNoIn1' -} }
                         | LeftHandSideExpression AssignmentOperator AssignmentExpressionNoIn
                           { AST.JSAssignExpression $1 $2 $3 {- 'AssignmentExpressionNoIn1' -} }

-- AssignmentOperator : one of                                                           See 11.13
--     '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '>>>=' | '&=' | '^=' | '|='
AssignmentOperator :: { AST.JSAssignOp }
AssignmentOperator : OpAssign     { $1 }
                   | SimpleAssign { AST.JSAssign $1 {- 'SimpleAssign' -} }

-- Expression :                                                   See 11.14
--         AssignmentExpression
--         Expression , AssignmentExpression
Expression :: { AST.JSExpression }
Expression : AssignmentExpression { $1 {- 'Expression' -} }
           | Expression Comma AssignmentExpression  { AST.JSCommaExpression $1 (nodePos $2) $3 {- 'Expression2' -} }

-- ExpressionNoIn :                                               See 11.14
--         AssignmentExpressionNoIn
--         ExpressionNoIn , AssignmentExpressionNoIn
ExpressionNoIn :: { AST.JSExpression }
ExpressionNoIn : AssignmentExpressionNoIn { $1 {- 'ExpressionNoIn' -} }
               | ExpressionNoIn Comma AssignmentExpressionNoIn  { AST.JSCommaExpression $1 (nodePos $2) $3 {- 'ExpressionNoIn2' -} }

-- TODO: still required?
ExpressionOpt :: { [AST.JSExpression] }
ExpressionOpt : Expression { [$1] {- 'ExpressionOpt' -} }
              |            { []   {- 'ExpressionOpt' -} }

ExpressionNoInOpt :: { [AST.JSExpression] }
ExpressionNoInOpt : ExpressionNoIn { [$1] {- 'ExpressionOpt' -} }
                  |                { []   {- 'ExpressionOpt' -} }


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
Statement :: { AST.JSStatement }
Statement : StatementNoEmpty   { $1 {- 'Statement1' -} }
          | EmptyStatement     { $1 {- 'Statement2' -} }

StatementNoEmpty :: { AST.JSStatement }
StatementNoEmpty : StatementBlock      { $1 {- 'StatementNoEmpty1' -} }
                 | VariableStatement   { $1 {- 'StatementNoEmpty2' -} }
                 | ExpressionStatement { $1 {- 'StatementNoEmpty4' -} }
                 | IfStatement         { $1 {- 'StatementNoEmpty5' -} }
                 | IterationStatement  { $1 {- 'StatementNoEmpty6' -} }
                 | ContinueStatement   { $1 {- 'StatementNoEmpty7' -} }
                 | BreakStatement      { $1 {- 'StatementNoEmpty8' -} }
                 | ReturnStatement     { $1 {- 'StatementNoEmpty9' -} }
                 | WithStatement       { $1 {- 'StatementNoEmpty10' -} }
                 | LabelledStatement   { $1 {- 'StatementNoEmpty11' -} }
                 | SwitchStatement     { $1 {- 'StatementNoEmpty12' -} }
                 | ThrowStatement      { $1 {- 'StatementNoEmpty13' -} }
                 | TryStatement        { $1 {- 'StatementNoEmpty14' -} }
                 | DebuggerStatement   { $1 {- 'StatementNoEmpty15' -} }


StatementBlock :: { AST.JSStatement }
StatementBlock : LBrace RBrace               { AST.JSStatementBlock (AST.JSBlock $1 [] $2) {- 'StatementBlock1' -} }
               | LBrace StatementList RBrace { AST.JSStatementBlock (AST.JSBlock $1 $2 $3) {- 'StatementBlock2' -} }

-- Block :                                                        See 12.1
--         { StatementListopt }
Block :: { AST.JSBlock }
Block : LBrace RBrace               { AST.JSBlock $1 [] $2 {- 'Block1' -} }
      | LBrace StatementList RBrace { AST.JSBlock $1 $2 $3 {- 'Block2' -} }

-- StatementList :                                                See 12.1
--         Statement
--         StatementList Statement
StatementList :: { [AST.JSStatement] }
StatementList : Statement               { [$1]       {- 'StatementList1' -} }
              | StatementList Statement { ($1++[$2]) {- 'StatementList2' -} }

-- VariableStatement :                                            See 12.2
--         var VariableDeclarationList ;
VariableStatement :: { AST.JSStatement }
VariableStatement : Var   VariableDeclarationList AutoSemi { AST.JSVariable $1 $2 $3 {- 'VariableStatement1' -} }
                  | Const VariableDeclarationList AutoSemi { AST.JSConstant $1 $2 $3 {- 'VariableStatement2' -} }

-- VariableDeclarationList :                                      See 12.2
--         VariableDeclaration
--         VariableDeclarationList , VariableDeclaration
VariableDeclarationList :: { [AST.JSStatement] }
VariableDeclarationList : VariableDeclaration { [$1] {- 'VariableDeclarationList1' -} }
                        | VariableDeclarationList Comma VariableDeclaration { ($1++[AST.JSExpressionStatement $2 AST.JSSemiAuto]++[$3]) {- 'VariableDeclarationList2' -} }

-- VariableDeclarationListNoIn :                                  See 12.2
--         VariableDeclarationNoIn
--         VariableDeclarationListNoIn , VariableDeclarationNoIn
VariableDeclarationListNoIn :: { [AST.JSStatement] }
VariableDeclarationListNoIn : VariableDeclarationNoIn { [$1] {- 'VariableDeclarationList3' -} }
                            | VariableDeclarationListNoIn Comma VariableDeclarationNoIn { ($1++[AST.JSExpressionStatement $2 AST.JSSemiAuto]++[$3]) {- 'VariableDeclarationListNoIn' -} }

-- VariableDeclaration :                                          See 12.2
--         Identifier Initialiseropt
VariableDeclaration :: { AST.JSStatement }
VariableDeclaration : Identifier SimpleAssign AssignmentExpression { AST.JSVarDecl $1 (AST.JSVarInit $2 $3) {- 'JSVarDeclInit1' -} }
                    | Identifier                                   { AST.JSVarDecl $1 AST.JSVarInitNone     {- 'JSVarDecl1' -} }

-- VariableDeclarationNoIn :                                      See 12.2
--         Identifier InitialiserNoInopt
VariableDeclarationNoIn :: { AST.JSStatement }
VariableDeclarationNoIn : Identifier SimpleAssign AssignmentExpression { AST.JSVarDecl $1 (AST.JSVarInit $2 $3) {- 'JSVarDeclInit2' -} }
                        | Identifier                                   { AST.JSVarDecl $1 AST.JSVarInitNone     {- 'JSVarDecl2' -} }

-- EmptyStatement :                                                                         See 12.3
--         ;
EmptyStatement :: { AST.JSStatement }
EmptyStatement : Semi { AST.JSEmptyStatement $1 {- 'EmptyStatement' -} }

-- ExpressionStatement :                                                                    See 12.4
--         [lookahead not in {{, function}] Expression  ;
-- TODO: Sort out lookahead issue. Maybe by just putting production lower to set reduce/reduce conflict
--       According to http://sideshowbarker.github.com/es5-spec/#x12.4, the ambiguity is with
--       Block or FunctionDeclaration
ExpressionStatement :: { AST.JSStatement }
ExpressionStatement : Expression AutoSemi { AST.JSExpressionStatement $1 $2 {- 'ExpressionStatement' -} }


-- IfStatement :                                                                            See 12.5
--         if ( Expression ) Statement else Statement
--         if ( Expression ) Statement
IfStatement :: { AST.JSStatement } -- +++XXXX++
IfStatement : If LParen Expression RParen Semi
                  { AST.JSIf $1 $2 $3 $4 (AST.JSEmptyStatement $5) {- 'IfStatement1' -} }
            | If LParen Expression RParen StatementNoEmpty Else Statement
                  {  AST.JSIfElse $1 $2 $3 $4 $5 $6 $7             {- 'IfStatement3' -} }
            | If LParen Expression RParen StatementNoEmpty
                  { AST.JSIf $1 $2 $3 $4 $5                        {- 'IfStatement3' -} }

-- IterationStatement :                                                                     See 12.6
--         do Statement while ( Expression );
--         while ( Expression ) Statement
--         for (ExpressionNoInopt; Expressionopt ; Expressionopt ) Statement
--         for ( var VariableDeclarationListNoIn; Expressionopt ; Expressionopt ) Statement
--         for ( LeftHandSideExpression in Expression ) Statement
--         for ( var VariableDeclarationNoIn in Expression ) Statement
IterationStatement :: { AST.JSStatement }
IterationStatement : Do StatementNoEmpty While LParen Expression RParen AutoSemi
                     { AST.JSDoWhile $1 $2 $3 $4 $5 $6 $7 {- 'IterationStatement1' -} }
                   | While LParen Expression RParen Statement
                     { AST.JSWhile $1 $2 $3 $4 $5 {- 'IterationStatement2' -} }
                   | For LParen ExpressionNoInOpt Semi ExpressionOpt Semi ExpressionOpt RParen Statement
                     { AST.JSFor $1 $2 $3 $4 $5 $6 $7 $8 $9 {- 'IterationStatement3' -} }
                   | For LParen Var VariableDeclarationListNoIn Semi ExpressionOpt Semi ExpressionOpt RParen Statement
                     { AST.JSForVar $1 $2 $3 $4 $5 $6 $7 $8 $9 $10 {- 'IterationStatement4' -} }
                   | For LParen LeftHandSideExpression In Expression RParen Statement
                     { AST.JSForIn $1 $2 $3 $4 $5 $6 $7 {- 'IterationStatement 5-} }
                   | For LParen Var VariableDeclarationNoIn In Expression RParen Statement
                     { AST.JSForVarIn $1 $2 $3 $4 $5 $6 $7 $8 {- 'IterationStatement6' -} }

-- ContinueStatement :                                                                      See 12.7
--         continue [no LineTerminator here] Identifieropt ;
-- TODO: deal with [no LineTerminator here]
ContinueStatement :: { AST.JSStatement }
ContinueStatement : Continue AutoSemi             { AST.JSContinue $1 AST.JSIdentNone $2  {- 'ContinueStatement1' -} }
                  | Continue Identifier AutoSemi  { AST.JSContinue $1 (identName $2) $3   {- 'ContinueStatement2' -} }

-- BreakStatement :                                                                         See 12.8
--         break [no LineTerminator here] Identifieropt ;
-- TODO: deal with [no LineTerminator here]
BreakStatement :: { AST.JSStatement }
BreakStatement : Break AutoSemi             { AST.JSBreak $1 AST.JSIdentNone $2 {- 'BreakStatement1' -} }
               | Break Identifier AutoSemi  { AST.JSBreak $1 (identName $2) $3  {- 'BreakStatement2' -} }

-- ReturnStatement :                                                                        See 12.9
--         return [no LineTerminator here] Expressionopt ;
-- TODO: deal with [no LineTerminator here]
ReturnStatement :: { AST.JSStatement }
ReturnStatement : Return AutoSemi             { AST.JSReturn $1 Nothing $2 }
                | Return Expression AutoSemi  { AST.JSReturn $1 (Just $2) $3 }

-- WithStatement :                                                                          See 12.10
--         with ( Expression ) Statement
WithStatement :: { AST.JSStatement }
WithStatement : With LParen Expression RParen Statement AutoSemi  { AST.JSWith $1 $2 $3 $4 $5 $6 }

-- SwitchStatement :                                                                        See 12.11
--         switch ( Expression ) CaseBlock
SwitchStatement :: { AST.JSStatement }
SwitchStatement : Switch LParen Expression RParen LBrace CaseBlock RBrace { AST.JSSwitch $1 $2 $3 $4 $5 $6 $7 }

-- CaseBlock :                                                                              See 12.11
--         { CaseClausesopt }
--         { CaseClausesopt DefaultClause CaseClausesopt }
CaseBlock :: { [AST.JSSwitchParts] }
CaseBlock : CaseClausesOpt                              { $1           {- 'CaseBlock1' -} }
          | CaseClausesOpt DefaultClause CaseClausesOpt { $1++[$2]++$3 {- 'CaseBlock2' -} }

-- CaseClauses :                                                                            See 12.11
--         CaseClause
--         CaseClauses CaseClause
CaseClausesOpt :: { [AST.JSSwitchParts] }
CaseClausesOpt : CaseClause                { [$1]       {- 'CaseClausesOpt1' -} }
               | CaseClausesOpt CaseClause { ($1++[$2]) {- 'CaseClausesOpt2' -} }
               |                           { []         {- 'CaseClausesOpt3' -} }

-- CaseClause :                                                               See 12.11
--        case Expression : StatementListopt
CaseClause :: { AST.JSSwitchParts }
CaseClause : Case Expression Colon StatementList  { AST.JSCase $1 $2 $3 $4 {- 'CaseClause1' -} }
           | Case Expression Colon                { AST.JSCase $1 $2 $3 [] {- 'CaseClause2' -} }

-- DefaultClause :                                                            See 12.11
--        default : StatementListopt
DefaultClause :: { AST.JSSwitchParts }
DefaultClause : Default Colon                { AST.JSDefault $1 $2 [] {- 'DefaultClause1' -} }
              | Default Colon StatementList  { AST.JSDefault $1 $2 $3 {- 'DefaultClause2' -} }

-- LabelledStatement :                                                        See 12.12
--        Identifier : Statement
LabelledStatement :: { AST.JSStatement }
LabelledStatement : Identifier Colon Statement { AST.JSLabelled $1 $2 $3 {- 'LabelledStatement' -} }

-- ThrowStatement :                                                           See 12.13
--        throw [no LineTerminator here] Expression ;
-- TODO : sort out no LineTerminator here
--        Does it need a semi at the end?
ThrowStatement :: { AST.JSStatement }
ThrowStatement : Throw Expression { AST.JSThrow $1 $2 {- 'ThrowStatement' -} }

-- Note: worked in updated syntax as per https://developer.mozilla.org/en/JavaScript/Reference/Statements/try...catch
--   i.e., 0 or more catches, then an optional finally
-- TryStatement :                                                             See 12.14
--        try Block Catch
--        try Block Finally
--        try Block Catch Finally
TryStatement :: { AST.JSStatement }
TryStatement : Try Block Catches         { AST.JSTry $1 $2 $3 AST.JSNoFinally {- 'TryStatement1' -} }
             | Try Block Finally         { AST.JSTry $1 $2 [] $3              {- 'TryStatement2' -} }
             | Try Block Catches Finally { AST.JSTry $1 $2 $3 $4              {- 'TryStatement3' -} }

Catches :: { [AST.JSTryCatch] }
Catches : Catch         { [$1]       {- 'Catches1' -} }
        | Catches Catch { ($1++[$2]) {- 'Catches2' -} }

-- Note: worked in updated syntax as per https://developer.mozilla.org/en/JavaScript/Reference/Statements/try...catch
-- <Catch> ::= 'catch' '(' Identifier ')' <Block>
--   becomes
-- <Catch> ::= 'catch' '(' Identifier ')' <Block>
--           | 'catch' '(' Identifier 'if' ConditionalExpression ')' <Block>
Catch :: { AST.JSTryCatch }
Catch : CatchL LParen Identifier                          RParen Block { AST.JSCatch $1 $2 $3 $4 $5 {- 'Catch1' -} }
      | CatchL LParen Identifier If ConditionalExpression RParen Block { AST.JSCatchIf $1 $2 $3 $4 $5 $6 $7 {- 'Catch2' -} }

-- Finally :                                                                  See 12.14
--        finally Block
Finally :: { AST.JSTryFinally }
Finally : FinallyL Block { AST.JSFinally $1 $2 {- 'Finally' -} }

-- DebuggerStatement :                                                        See 12.15
--        debugger ;
DebuggerStatement :: { AST.JSStatement }
DebuggerStatement : 'debugger' AutoSemi { AST.JSExpressionStatement (AST.JSLiteral (AST.JSAnnot (ss $1) (gc $1)) "debugger") $2 {- 'DebuggerStatement' -} }

-- FunctionDeclaration :                                                      See clause 13
--        function Identifier ( FormalParameterListopt ) { FunctionBody }
FunctionDeclaration :: { AST.JSStatement }
FunctionDeclaration : Function Identifier LParen FormalParameterList RParen FunctionBody
                      { AST.JSFunction $1 (identName $2) $3 (AST.JSParams $4) $5 $6 {- 'FunctionDeclaration1' -} }
                    | Function Identifier LParen RParen FunctionBody
                      { AST.JSFunction $1 (identName $2) $3 AST.JSNoParams $4 $5 {- 'FunctionDeclaration2' -} }

-- FunctionExpression :                                                       See clause 13
--        function Identifieropt ( FormalParameterListopt ) { FunctionBody }
FunctionExpression :: { AST.JSExpression }
FunctionExpression : Function IdentifierOpt LParen RParen FunctionBody
                     { AST.JSFunctionExpression $1 $2 $3 AST.JSNoParams $4 $5 {- 'FunctionExpression1' -} }
                   | Function IdentifierOpt LParen FormalParameterList RParen FunctionBody
                     { AST.JSFunctionExpression $1 $2 $3 (AST.JSParams $4) $5 $6 {- 'FunctionExpression2' -} }

IdentifierOpt :: { AST.JSIdent }
IdentifierOpt : Identifier { identName $1     {- 'IdentifierOpt1' -} }
              |            { AST.JSIdentNone  {- 'IdentifierOpt2' -} }

-- FormalParameterList :                                                      See clause 13
--        Identifier
--        FormalParameterList , Identifier
FormalParameterList :: { AST.JSNonEmptyList AST.JSIdent }
FormalParameterList : Identifier                            { AST.JSLOne (identName $1) {- 'FormalParameterList' -} }
                    | FormalParameterList Comma Identifier  { AST.JSLCons $1 (nodePos $2) (identName $3) }

-- FunctionBody :                                                             See clause 13
--        SourceElementsopt
FunctionBody :: { AST.JSBlock }
FunctionBody : LBrace SourceElements RBrace { AST.JSBlock $1 $2 $3 {- 'FunctionBody1' -} }
             | LBrace                RBrace { AST.JSBlock $1 [] $2 {- 'FunctionBody2' -} }

-- Program :                                                                  See clause 14
--        SourceElementsopt

Program :: { AST.JSAST }
Program : SourceElementsTop Eof { combineTop $1 $2             {- 'Program1' -} }
        | Eof                   { AST.JSSourceElementsTop [$1] {- 'Program2' -} }

-- For debugging/other entry points
LiteralMain :: { AST.JSAST }
LiteralMain : Literal Eof { AST.JSSourceElementsTop [AST.JSExpressionStatement $1 AST.JSSemiAuto] {- 'LiteralMain' -} }

PrimaryExpressionMain :: { AST.JSAST }
PrimaryExpressionMain : PrimaryExpression Eof { AST.JSSourceElementsTop [AST.JSExpressionStatement $1 AST.JSSemiAuto] {- 'PrimaryExpression' -} }

StatementMain :: { AST.JSAST }
StatementMain : Statement Eof { AST.JSSourceElementsTop [$1] {- 'StatementMain' -} }


-- SourceElements :                                                           See clause 14
--        SourceElement
--        SourceElements SourceElement
SourceElements :: { [AST.JSStatement] }
SourceElements : SourceElement                { [$1]     {- 'SourceElements1' -} }
               | SourceElements SourceElement { $1++[$2] {- 'SourceElements2' -} }

SourceElementsTop :: { AST.JSAST }
SourceElementsTop : SourceElement                   { AST.JSSourceElementsTop [$1]     {- 'SourceElementsTop1' -} }
                  | SourceElementsTop SourceElement { (combineSourceElementsTop $1 $2) {- 'SourceElementsTop2' -} }

-- SourceElement :
--       Statement
--       FunctionDeclaration
SourceElement :: { AST.JSStatement }
SourceElement : Statement            { $1 {- 'SourceElement1' -} }
              | FunctionDeclaration  { $1 {- 'SourceElement2' -} }

{
combineSourceElementsTop :: AST.JSAST -> AST.JSStatement -> AST.JSAST
combineSourceElementsTop (AST.JSSourceElementsTop xs) x1 = AST.JSSourceElementsTop (xs++[x1])

combineTop :: AST.JSAST -> AST.JSStatement -> AST.JSAST
combineTop (AST.JSSourceElementsTop xs) x1 = AST.JSSourceElementsTop (xs++[x1])


parseError :: Token -> Alex a
parseError tok = alexError (show tok)

-- --------------------------------

ss :: Token -> TokenPosn
ss = tokenSpan

-- ------------------------------

gc :: Token -> [CommentAnnotation]
gc = tokenComment
mgc :: [Token] -> [CommentAnnotation]
mgc xs = concatMap tokenComment xs

-- ---------------------------------------------------------------------
-- | mkUnary : The parser detects '+' and '-' as the binary version of these
-- operator. This function converts from the binary version to the unary
-- version.
mkUnary :: AST.JSBinOp -> AST.JSUnaryOp
mkUnary (AST.JSBinOpMinus annot) = AST.JSUnaryOpMinus annot
mkUnary (AST.JSBinOpPlus  annot) = AST.JSUnaryOpPlus  annot

mkUnary x = error $ "Invalid unary op : " ++ show x

nodePos :: AST.JSExpression -> AST.JSAnnot
nodePos (AST.JSComma p) = p

identName :: AST.JSExpression -> AST.JSIdent
identName (AST.JSIdentifier a s) = AST.JSIdentName a s
identName x = error $ "Cannot convert '" ++ show x ++ "' to s JSIdentName."

}
