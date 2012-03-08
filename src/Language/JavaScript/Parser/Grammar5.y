{
module Language.JavaScript.Parser.Grammar5 (
    parseProgram
  , parseLiteral
  , parsePrimaryExpression
  , parseStatement
  ) where

-- import Control.Monad.Error.Class (throwError)
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
%name parseLiteral           Literal
%name parsePrimaryExpression PrimaryExpression
%name parseStatement         Statement

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
     'string'     { StringToken {} }
     'regex'      { RegExToken {} }
     'assign'     { AssignToken {} }

     'future'     { FutureToken {} }


%%

-- ---------------------------------------------------------------------
-- Sort out automatically inserted semi-colons

AutoSemi :: { AST.JSNode }
AutoSemi : ';' { AST.NS (AST.JSLiteral ";") (ss $1) (gc $1)}
         |     { AST.NS (AST.JSLiteral "") tokenPosnEmpty []}

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
NullLiteral : 'null' { fp (AST.NS (AST.JSLiteral "null") (ss $1) (gc $1))}

BooleanLiteral :: { AST.JSNode }
BooleanLiteral : 'true'  { fp (AST.NS (AST.JSLiteral "true")  (ss $1) (gc $1)) }
               | 'false' { fp (AST.NS (AST.JSLiteral "false") (ss $1) (gc $1)) }

-- <Numeric Literal> ::= DecimalLiteral
--                     | HexIntegerLiteral
NumericLiteral :: { AST.JSNode }
NumericLiteral : 'decimal'    { fp (AST.NS (AST.JSDecimal (token_literal $1)) (ss $1) (gc $1))}
               | 'hexinteger' { fp (AST.NS (AST.JSHexInteger (token_literal $1)) (ss $1) (gc $1)) }

StringLiteral :: { AST.JSNode }
StringLiteral : 'string'  { fp (AST.NS (AST.JSStringLiteral (token_delimiter $1) (token_literal $1)) (ss $1) (gc $1)) }

-- <Regular Expression Literal> ::= RegExp
RegularExpressionLiteral :: { AST.JSNode }
RegularExpressionLiteral : 'regex' { fp (AST.NS (AST.JSRegEx (token_literal $1)) (ss $1) (gc $1)) }

-- PrimaryExpression :                                                   See 11.1
--        this
--        Identifier
--        Literal
--        ArrayLiteral
--        ObjectLiteral
--        ( Expression )
PrimaryExpression :: { AST.JSNode }
PrimaryExpression : 'this'                   { fp (AST.NS (AST.JSLiteral "this") (ss $1) (gc $1))}
                  | Identifier               { $1 {- PrimaryExpression1 -}}
                  | Literal                  { $1 {- PrimaryExpression2 -}}
                  | ArrayLiteral             { $1 {- PrimaryExpression3 -}}
                  | ObjectLiteral            { $1 {- PrimaryExpression4 -}}
                  | '(' Expression ')'       { fp (AST.NS (AST.JSExpressionParen $2) (ss $1) ((gc $1)++(gnc $2)++(gc $3)))}

-- Identifier ::                                                            See 7.6
--         IdentifierName but not ReservedWord
-- IdentifierName ::                                                        See 7.6
--         IdentifierStart
--         IdentifierName IdentifierPart
Identifier :: { AST.JSNode }
Identifier : 'ident' {  (AST.NS (AST.JSIdentifier (token_literal $1)) (ss $1) (gc $1))}
           | 'get'   {  (AST.NS (AST.JSIdentifier "get") (ss $1) (gc $1))}
           | 'set'   {  (AST.NS (AST.JSIdentifier "set") (ss $1) (gc $1))}

-- TODO: make this include any reserved word too, including future ones
IdentifierName :: { AST.JSNode }
IdentifierName : Identifier {$1}
             | 'break'      { fp (AST.NS (AST.JSIdentifier "break") (ss $1) (gc $1))}
             | 'case'       { fp (AST.NS (AST.JSIdentifier "case") (ss $1) (gc $1))}
             | 'catch'      { fp (AST.NS (AST.JSIdentifier "catch") (ss $1) (gc $1))}
             | 'const'      { fp (AST.NS (AST.JSIdentifier "const") (ss $1) (gc $1))}
             | 'continue'   { fp (AST.NS (AST.JSIdentifier "continue") (ss $1) (gc $1))}
             | 'debugger'   { fp (AST.NS (AST.JSIdentifier "debugger") (ss $1) (gc $1))}
             | 'default'    { fp (AST.NS (AST.JSIdentifier "default") (ss $1) (gc $1))}
             | 'delete'     { fp (AST.NS (AST.JSIdentifier "delete") (ss $1) (gc $1))}
             | 'do'         { fp (AST.NS (AST.JSIdentifier "do") (ss $1) (gc $1))}
             | 'else'       { fp (AST.NS (AST.JSIdentifier "else") (ss $1) (gc $1))}
             | 'enum'       { fp (AST.NS (AST.JSIdentifier "enum") (ss $1) (gc $1))}
             | 'false'      { fp (AST.NS (AST.JSIdentifier "false") (ss $1) (gc $1))}
             | 'finally'    { fp (AST.NS (AST.JSIdentifier "finally") (ss $1) (gc $1))}
             | 'for'        { fp (AST.NS (AST.JSIdentifier "for")  (ss $1) (gc $1))}
             | 'function'   { fp (AST.NS (AST.JSIdentifier "function") (ss $1) (gc $1))}
             | 'get'        { fp (AST.NS (AST.JSIdentifier "get") (ss $1) (gc $1))}
             | 'if'         { fp (AST.NS (AST.JSIdentifier "if") (ss $1) (gc $1))}
             | 'in'         { fp (AST.NS (AST.JSIdentifier "in") (ss $1) (gc $1))}
             | 'instanceof' { fp (AST.NS (AST.JSIdentifier "instanceof") (ss $1) (gc $1))}
             | 'new'        { fp (AST.NS (AST.JSIdentifier "new") (ss $1) (gc $1))}
             | 'null'       { fp (AST.NS (AST.JSIdentifier "null") (ss $1) (gc $1))}
             | 'return'     { fp (AST.NS (AST.JSIdentifier "return") (ss $1) (gc $1))}
             | 'set'        { fp (AST.NS (AST.JSIdentifier "set") (ss $1) (gc $1))}
             | 'switch'     { fp (AST.NS (AST.JSIdentifier "switch") (ss $1) (gc $1))}
             | 'this'       { fp (AST.NS (AST.JSIdentifier "this") (ss $1) (gc $1))}
             | 'throw'      { fp (AST.NS (AST.JSIdentifier "throw") (ss $1) (gc $1))}
             | 'true'       { fp (AST.NS (AST.JSIdentifier "true") (ss $1) (gc $1))}
             | 'try'        { fp (AST.NS (AST.JSIdentifier "try") (ss $1) (gc $1))}
             | 'typeof'     { fp (AST.NS (AST.JSIdentifier "typeof") (ss $1) (gc $1))}
             | 'var'        { fp (AST.NS (AST.JSIdentifier "var") (ss $1) (gc $1))}
             | 'void'       { fp (AST.NS (AST.JSIdentifier "void") (ss $1) (gc $1))}
             | 'while'      { fp (AST.NS (AST.JSIdentifier "while") (ss $1) (gc $1))}
             | 'with'       { fp (AST.NS (AST.JSIdentifier "with") (ss $1) (gc $1))}
             | 'future'     { fp (AST.NS (AST.JSIdentifier (token_literal $1)) (ss $1) (gc $1))}



-- ArrayLiteral :                                                        See 11.1.4
--        [ Elisionopt ]
--        [ ElementList ]
--        [ ElementList , Elisionopt ]
ArrayLiteral :: { AST.JSNode }
ArrayLiteral : '[' ']'                         { fp (AST.NS (AST.JSArrayLiteral []) (ss $1) ((gc $1)++(gc $2)))}
             | '[' Elision ']'                 { fp (AST.NS (AST.JSArrayLiteral $2) (ss $1) (mgc [$1,$3]))}
             | '[' ElementList ']'             { fp (AST.NS (AST.JSArrayLiteral $2) (ss $1) (mgc [$1,$3]))}
             | '[' ElementList ',' Elision ']' { fp (AST.NS (AST.JSArrayLiteral ($2++$4)) (ss $1) (mgc [$1,$3,$5]))}
             | '[' ElementList ',' ']'         { fp (AST.NS (AST.JSArrayLiteral ($2++[AST.NS (AST.JSLiteral ",") (ss $3) (gc $3)])) (ss $1) (mgc [$1,$4]))}

-- ElementList :                                                         See 11.1.4
--        Elisionopt AssignmentExpression
--        ElementList , Elisionopt AssignmentExpression
ElementList :: { [AST.JSNode] }
ElementList : Elision AssignmentExpression                 { (($1)++($2)) {- ElementList -}}
            | AssignmentExpression                         { $1           {- ElementList -}}
            | ElementList ',' Elision AssignmentExpression { (($1)++[fp (AST.NS (AST.JSElision []) (ss $2) (gc $2))]++($3)++($4)) {- ElementList -}}
            | ElementList ',' AssignmentExpression         { (($1)++[fp (AST.NS (AST.JSElision []) (ss $2) (gc $2))]++($3)) {- ElementList -}}

-- Elision :                                                             See 11.1.4
--        ,
--        Elision ,
Elision :: { [AST.JSNode] }
Elision :  ','        { [fp (AST.NS (AST.JSElision []) tokenPosnEmpty (gc $1))] }
        | Elision ',' { ($1 ++ [fp (AST.NS (AST.JSElision []) tokenPosnEmpty (gc $2))]) }

-- ObjectLiteral :                                                       See 11.1.5
--        { }
--        { PropertyNameAndValueList }
--        { PropertyNameAndValueList , }
ObjectLiteral :: { AST.JSNode }
ObjectLiteral : '{' '}'                          { fp (AST.NS (AST.JSObjectLiteral []) (ss $1) ((gc $1)++(gc $2)))}
              | '{' PropertyNameandValueList '}' { fp (AST.NS (AST.JSObjectLiteral $2) (ss $1) ((gc $1)++(gc $3)))}
              | '{' PropertyNameandValueList ',' '}' { fp (AST.NS (AST.JSObjectLiteral ($2++[AST.NS (AST.JSLiteral ",") (ss $3) (gc $3)])) (ss $1) ((gc $1)++(gc $4)))}

-- <Property Name and Value List> ::= <Property Name> ':' <Assignment Expression>
--                                  | <Property Name and Value List> ',' <Property Name> ':' <Assignment Expression>

-- Seems we can have function declarations in the value part too
-- PropertyNameAndValueList :                                            See 11.1.5
--        PropertyAssignment
--        PropertyNameAndValueList , PropertyAssignment
PropertyNameandValueList :: { [ AST.JSNode ] }
PropertyNameandValueList : PropertyAssignment                              { [$1] {- PropertyNameandValueList1 -} }
                         | PropertyNameandValueList ',' PropertyAssignment { ($1 ++ [(pc $3 (gc $2))]) {- PropertyNameandValueList2 -} }

-- PropertyAssignment :                                                  See 11.1.5
--        PropertyName : AssignmentExpression
--        get PropertyName() { FunctionBody }
--        set PropertyName( PropertySetParameterList ) { FunctionBody }
-- TODO: not clear if get/set are keywords, or just used in a specific context. Puzzling.
PropertyAssignment :: { AST.JSNode }
PropertyAssignment : PropertyName ':' AssignmentExpression { fp (AST.NS (AST.JSPropertyNameandValue $1 $3) (ex $1) (gc $2)) }
                   -- Should be "get" in next, but is not a Token
                   | 'get' PropertyName '(' ')' '{' FunctionBody '}'
                       { fp (AST.NS (AST.JSPropertyAccessor "get" $2 [] $6) (ss $1) (mgc [$1,$3,$4,$5,$7])) }
                   -- Should be "set" in next, but is not a Token
                   | 'set' PropertyName '(' PropertySetParameterList ')' '{' FunctionBody '}'
                       { fp (AST.NS (AST.JSPropertyAccessor "set" $2 [$4] $7) (ss $1) (mgc [$1,$3,$5,$6,$8])) }

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
                 | MemberExpression '[' Expression ']' { [fp (AST.NS (AST.JSMemberSquare $1 $3) (mex $1) (mgc [$2,$4]))] }
                 | MemberExpression '.' IdentifierName { [fp (AST.NS (AST.JSMemberDot $1 $3) (mex $1) (gc $2))] }
                 | 'new' MemberExpression Arguments    { (((fp (AST.NS (AST.JSLiteral "new ") (ss $1) (gc $1))):$2)++[$3])}

-- NewExpression :                                              See 11.2
--        MemberExpression
--        new NewExpression
NewExpression :: { [AST.JSNode] }
NewExpression : MemberExpression {$1 {- NewExpression -}}
              | 'new' NewExpression { (fp (AST.NS (AST.JSLiteral "new ") (ss $1) (gc $1))):$2 }

-- CallExpression :                                             See 11.2
--        MemberExpression Arguments
--        CallExpression Arguments
--        CallExpression [ Expression ]
--        CallExpression . IdentifierName
CallExpression :: { [AST.JSNode] }
CallExpression : MemberExpression Arguments        { $1++[$2] {- CallExpression -} }
               | CallExpression Arguments          { ($1++[fp (AST.NS (AST.JSCallExpression "()" [$2]) (mex $1) [])]) }
               | CallExpression '[' Expression ']' { ($1++[fp (AST.NS (AST.JSCallExpression "[]" [$3]) (mex $1) (mgc [$2,$4]))]) }
               | CallExpression '.' IdentifierName { ($1++[fp (AST.NS (AST.JSCallExpression "."  [$3]) (mex $1) (gc $2))]) }

-- Arguments :                                                  See 11.2
--        ()
--        ( ArgumentList )
Arguments :: { AST.JSNode }
Arguments : '(' ')'               { fp (AST.NS (AST.JSArguments []) (ss $1) (mgc [$1,$2])) }
          | '(' ArgumentList ')'  { fp (AST.NS (AST.JSArguments $2) (ss $1) (mgc [$1,$3])) }

-- ArgumentList :                                               See 11.2
--        AssignmentExpression
--        ArgumentList , AssignmentExpression
ArgumentList :: { [[AST.JSNode]] }
ArgumentList : AssignmentExpression { [$1] {- ArgumentList -}}
             | ArgumentList ',' AssignmentExpression { $1++[ (mpc $3 (gc $2)) ] {- ArgumentList2 -} }

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
                  | PostfixExpression '++' {[fp (AST.NS (AST.JSExpressionPostfix "++" $1) (mex $1) (gc $2))]}
                  | PostfixExpression '--' {[fp (AST.NS (AST.JSExpressionPostfix "--" $1) (mex $1) (gc $2))]}

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
                | 'delete' UnaryExpression { ((fp (AST.NS (AST.JSUnary "delete ") (ss $1) (gc $1))):$2)}
                | 'void'   UnaryExpression { ((fp (AST.NS (AST.JSUnary "void ") (ss $1) (gc $1))):$2)}
                | 'typeof' UnaryExpression { ((fp (AST.NS (AST.JSUnary "typeof ") (ss $1) (gc $1))):$2)}
                | '++'     UnaryExpression { ((fp (AST.NS (AST.JSUnary "++") (ss $1) (gc $1))):$2) }
                | '--'     UnaryExpression { ((fp (AST.NS (AST.JSUnary "--") (ss $1) (gc $1))):$2)}
                | '+'      UnaryExpression { ((fp (AST.NS (AST.JSUnary "+") (ss $1) (gc $1))):$2)}
                | '-'      UnaryExpression { ((fp (AST.NS (AST.JSUnary "-") (ss $1) (gc $1))):$2)}
                | '~'      UnaryExpression { ((fp (AST.NS (AST.JSUnary "~") (ss $1) (gc $1))):$2)}
                | '!'      UnaryExpression { ((fp (AST.NS (AST.JSUnary "!") (ss $1) (gc $1))):$2)}

-- MultiplicativeExpression :                                   See 11.5
--        UnaryExpression
--        MultiplicativeExpression * UnaryExpression
--        MultiplicativeExpression / UnaryExpression
--        MultiplicativeExpression % UnaryExpression
MultiplicativeExpression :: { [AST.JSNode] }
MultiplicativeExpression : UnaryExpression { $1 {- MultiplicativeExpression -}}
                         | MultiplicativeExpression '*' UnaryExpression { [fp (AST.NS (AST.JSExpressionBinary "*" $1 $3) (mex $1) (gc $2))]}
                         | MultiplicativeExpression '/' UnaryExpression { [fp (AST.NS (AST.JSExpressionBinary "/" $1 $3) (mex $1) (gc $2))]}
                         | MultiplicativeExpression '%' UnaryExpression { [fp (AST.NS (AST.JSExpressionBinary "%" $1 $3) (mex $1) (gc $2))]}

-- AdditiveExpression :                                        See 11.6
--        MultiplicativeExpression
--        AdditiveExpression + MultiplicativeExpression
--        AdditiveExpression - MultiplicativeExpression
AdditiveExpression :: { [AST.JSNode] }
AdditiveExpression : AdditiveExpression '+' MultiplicativeExpression { [fp (AST.NS (AST.JSExpressionBinary "+" $1 $3) (mex $1) (gc $2))]}
                   | AdditiveExpression '-' MultiplicativeExpression { [fp (AST.NS (AST.JSExpressionBinary "-" $1 $3) (mex $1) (gc $2))]}
                   | MultiplicativeExpression { $1 {- (goRegExp $1)-} {- AdditiveExpression -} }

-- ShiftExpression :                                           See 11.7
--        AdditiveExpression
--        ShiftExpression << AdditiveExpression
--        ShiftExpression >> AdditiveExpression
--        ShiftExpression >>> AdditiveExpression
ShiftExpression :: { [AST.JSNode] }
ShiftExpression : ShiftExpression '<<'  AdditiveExpression { [fp (AST.NS (AST.JSExpressionBinary "<<" $1 $3) (mex $1) (gc $2))]}
                | ShiftExpression '>>'  AdditiveExpression { [fp (AST.NS (AST.JSExpressionBinary ">>" $1 $3) (mex $1) (gc $2))]}
                | ShiftExpression '>>>' AdditiveExpression { [fp (AST.NS (AST.JSExpressionBinary ">>>" $1 $3) (mex $1) (gc $2))]}
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
                     | RelationalExpression '<'  ShiftExpression { [fp (AST.NS (AST.JSExpressionBinary "<" $1 $3) (mex $1) (gc $2))]}
                     | RelationalExpression '>'  ShiftExpression { [fp (AST.NS (AST.JSExpressionBinary ">" $1 $3) (mex $1) (gc $2))]}
                     | RelationalExpression '<=' ShiftExpression { [fp (AST.NS (AST.JSExpressionBinary "<=" $1 $3) (mex $1) (gc $2))]}
                     | RelationalExpression '>=' ShiftExpression { [fp (AST.NS (AST.JSExpressionBinary ">=" $1 $3) (mex $1) (gc $2))]}
                     | RelationalExpression 'instanceof' ShiftExpression { [fp (AST.NS (AST.JSExpressionBinary " instanceof " $1 $3) (mex $1) (gc $2))]}
                     | RelationalExpression 'in' ShiftExpression { [fp (AST.NS (AST.JSExpressionBinary " in " $1 $3) (mex $1) (gc $2))]}

-- RelationalExpressionNoIn :                                  See 11.8
--        ShiftExpression
--        RelationalExpressionNoIn < ShiftExpression
--        RelationalExpressionNoIn > ShiftExpression
--        RelationalExpressionNoIn <= ShiftExpression
--        RelationalExpressionNoIn >= ShiftExpression
--        RelationalExpressionNoIn instanceof ShiftExpression
RelationalExpressionNoIn :: { [AST.JSNode] }
RelationalExpressionNoIn : ShiftExpression { $1 {- RelationalExpressionNoIn -}}
                     | RelationalExpressionNoIn '<'  ShiftExpression { [fp (AST.NS (AST.JSExpressionBinary "<" $1 $3) (mex $1) (gc $2))]}
                     | RelationalExpressionNoIn '>'  ShiftExpression { [fp (AST.NS (AST.JSExpressionBinary ">" $1 $3) (mex $1) (gc $2))]}
                     | RelationalExpressionNoIn '<=' ShiftExpression { [fp (AST.NS (AST.JSExpressionBinary "<=" $1 $3) (mex $1) (gc $2))]}
                     | RelationalExpressionNoIn '>=' ShiftExpression { [fp (AST.NS (AST.JSExpressionBinary ">=" $1 $3) (mex $1) (gc $2))]}
                     | RelationalExpressionNoIn 'instanceof' ShiftExpression { [fp (AST.NS (AST.JSExpressionBinary " instanceof " $1 $3) (mex $1) (gc $2))]}

-- EqualityExpression :                                        See 11.9
--        RelationalExpression
--        EqualityExpression == RelationalExpression
--        EqualityExpression != RelationalExpression
--        EqualityExpression === RelationalExpression
--        EqualityExpression !== RelationalExpression
EqualityExpression :: { [AST.JSNode] }
EqualityExpression : RelationalExpression { $1 {- EqualityExpression -} }
                   | EqualityExpression '=='  RelationalExpression { [fp (AST.NS (AST.JSExpressionBinary "==" $1 $3) (mex $1) (gc $2))]}
                   | EqualityExpression '!='  RelationalExpression { [fp (AST.NS (AST.JSExpressionBinary "!=" $1 $3) (mex $1) (gc $2))]}
                   | EqualityExpression '===' RelationalExpression { [fp (AST.NS (AST.JSExpressionBinary "===" $1 $3) (mex $1) (gc $2))]}
                   | EqualityExpression '!==' RelationalExpression { [fp (AST.NS (AST.JSExpressionBinary "!==" $1 $3) (mex $1) (gc $2))]}

-- EqualityExpressionNoIn :                                    See 11.9
--        RelationalExpressionNoIn
--        EqualityExpressionNoIn == RelationalExpressionNoIn
--        EqualityExpressionNoIn != RelationalExpressionNoIn
--        EqualityExpressionNoIn === RelationalExpressionNoIn
--        EqualityExpressionNoIn !== RelationalExpressionNoIn
EqualityExpressionNoIn :: { [AST.JSNode] }
EqualityExpressionNoIn : RelationalExpressionNoIn { $1 {- EqualityExpressionNoIn -} }
                       | EqualityExpressionNoIn '=='  RelationalExpression { [fp (AST.NS (AST.JSExpressionBinary "==" $1 $3) (mex $1) (gc $2))]}
                       | EqualityExpressionNoIn '!='  RelationalExpression { [fp (AST.NS (AST.JSExpressionBinary "!=" $1 $3) (mex $1) (gc $2))]}
                       | EqualityExpressionNoIn '===' RelationalExpression { [fp (AST.NS (AST.JSExpressionBinary "===" $1 $3) (mex $1) (gc $2))]}
                       | EqualityExpressionNoIn '!==' RelationalExpression { [fp (AST.NS (AST.JSExpressionBinary "!==" $1 $3) (mex $1) (gc $2))]}

-- BitwiseANDExpression :                                      See 11.10
--        EqualityExpression
--        BitwiseANDExpression & EqualityExpression
BitwiseAndExpression :: { [AST.JSNode] }
BitwiseAndExpression : EqualityExpression { $1 {- BitwiseAndExpression -} }
                     | BitwiseAndExpression '&' EqualityExpression { [fp (AST.NS (AST.JSExpressionBinary "&" $1 $3) (mex $1) (gc $2))]}

-- BitwiseANDExpressionNoIn :                                  See 11.10
--        EqualityExpressionNoIn
--        BitwiseANDExpressionNoIn & EqualityExpressionNoIn
BitwiseAndExpressionNoIn :: { [AST.JSNode] }
BitwiseAndExpressionNoIn : EqualityExpressionNoIn { $1 {- BitwiseAndExpression -} }
                     | BitwiseAndExpressionNoIn '&' EqualityExpressionNoIn { [fp (AST.NS (AST.JSExpressionBinary "&" $1 $3) (mex $1) (gc $2))]}

-- BitwiseXORExpression :                                                                See 11.10
--        BitwiseANDExpression
--        BitwiseXORExpression ^ BitwiseANDExpression
BitwiseXOrExpression :: { [AST.JSNode] }
BitwiseXOrExpression : BitwiseAndExpression { $1 {- BitwiseXOrExpression -} }
                     | BitwiseXOrExpression '^' BitwiseAndExpression { [fp (AST.NS (AST.JSExpressionBinary "^" $1 $3) (mex $1) (gc $2))]}

-- BitwiseXORExpressionNoIn :                                                            See 11.10
--        BitwiseANDExpressionNoIn
--        BitwiseXORExpressionNoIn ^ BitwiseANDExpressionNoIn
BitwiseXOrExpressionNoIn :: { [AST.JSNode] }
BitwiseXOrExpressionNoIn : BitwiseAndExpressionNoIn { $1 {- BitwiseXOrExpression -} }
                         | BitwiseXOrExpressionNoIn '^' BitwiseAndExpressionNoIn { [fp (AST.NS (AST.JSExpressionBinary "^" $1 $3) (mex $1) (gc $2))]}

-- BitwiseORExpression :                                                                 See 11.10
--        BitwiseXORExpression
--        BitwiseORExpression | BitwiseXORExpression
BitwiseOrExpression :: { [AST.JSNode] }
BitwiseOrExpression : BitwiseXOrExpression { $1 {- BitwiseOrExpression -} }
                    | BitwiseOrExpression '|' BitwiseXOrExpression { [fp (AST.NS (AST.JSExpressionBinary "|" $1 $3) (mex $1) (gc $2))]}

-- BitwiseORExpressionNoIn :                                                             See 11.10
--        BitwiseXORExpressionNoIn
--        BitwiseORExpressionNoIn | BitwiseXORExpressionNoIn
BitwiseOrExpressionNoIn :: { [AST.JSNode] }
BitwiseOrExpressionNoIn : BitwiseXOrExpressionNoIn { $1 {- BitwiseOrExpression -} }
                        | BitwiseOrExpressionNoIn '|' BitwiseXOrExpressionNoIn { [fp (AST.NS (AST.JSExpressionBinary "|" $1 $3) (mex $1) (gc $2))]}

-- LogicalANDExpression :                                                                See 11.11
--        BitwiseORExpression
--        LogicalANDExpression && BitwiseORExpression
LogicalAndExpression :: { [AST.JSNode] }
LogicalAndExpression : BitwiseOrExpression { $1 {- LogicalAndExpression -} }
                     | LogicalAndExpression '&&' BitwiseOrExpression { [fp (AST.NS (AST.JSExpressionBinary "&&" $1 $3) (mex $1) (gc $2))]}

-- LogicalANDExpressionNoIn :                                                            See 11.11
--        BitwiseORExpressionNoIn
--        LogicalANDExpressionNoIn && BitwiseORExpressionNoIn
LogicalAndExpressionNoIn :: { [AST.JSNode] }
LogicalAndExpressionNoIn : BitwiseOrExpressionNoIn { $1 {- LogicalAndExpression -} }
                         | LogicalAndExpressionNoIn '&&' BitwiseOrExpressionNoIn { [fp (AST.NS (AST.JSExpressionBinary "&&" $1 $3) (mex $1) (gc $2))]}

-- LogicalORExpression :                                                                 See 11.11
--        LogicalANDExpression
--        LogicalORExpression || LogicalANDExpression
LogicalOrExpression :: { [AST.JSNode] }
LogicalOrExpression : LogicalAndExpression { $1 {- LogicalOrExpression -} }
                    | LogicalOrExpression '||' LogicalAndExpression { [fp (AST.NS (AST.JSExpressionBinary "||" $1 $3) (mex $1) (gc $2))]}

-- LogicalORExpressionNoIn :                                                             See 11.11
--        LogicalANDExpressionNoIn
--        LogicalORExpressionNoIn || LogicalANDExpressionNoIn
LogicalOrExpressionNoIn :: { [AST.JSNode] }
LogicalOrExpressionNoIn : LogicalAndExpressionNoIn { $1 {- LogicalOrExpression -} }
                        | LogicalOrExpressionNoIn '||' LogicalAndExpressionNoIn { [fp (AST.NS (AST.JSExpressionBinary "||" $1 $3) (mex $1) (gc $2))]}

-- ConditionalExpression :                                                               See 11.12
--        LogicalORExpression
--        LogicalORExpression ? AssignmentExpression : AssignmentExpression
ConditionalExpression :: { [AST.JSNode] }
ConditionalExpression : LogicalOrExpression { $1 {- ConditionalExpression -} }
                      | LogicalOrExpression '?' AssignmentExpression ':' AssignmentExpression
                        { [fp (AST.NS (AST.JSExpressionTernary $1 $3 $5) (mex $1) (mgc [$2,$4]))] }

-- ConditionalExpressionNoIn :                                                           See 11.12
--        LogicalORExpressionNoIn
--        LogicalORExpressionNoIn ? AssignmentExpressionNoIn : AssignmentExpressionNoIn
ConditionalExpressionNoIn :: { [AST.JSNode] }
ConditionalExpressionNoIn : LogicalOrExpressionNoIn { $1 {- ConditionalExpression -} }
                          | LogicalOrExpressionNoIn '?' AssignmentExpressionNoIn ':' AssignmentExpressionNoIn
                            { [fp (AST.NS (AST.JSExpressionTernary $1 $3 $5) (mex $1) (mgc [$2,$4]))] }

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
AssignmentOperator : 'assign' { fp (AST.NS (AST.JSOperator (token_literal $1)) (ss $1) (gc $1))}
                   | '='      { fp (AST.NS (AST.JSOperator "=") (ss $1) (gc $1))}

-- Expression :                                                   See 11.14
--         AssignmentExpression
--         Expression , AssignmentExpression
Expression :: { AST.JSNode }
Expression : AssignmentExpression { fp (AST.NS (AST.JSExpression $1) (mex $1) []) {- Expression -} }
           | Expression ',' AssignmentExpression  { flattenExpression $1 (mpc $3 (gc $2)) }

-- ExpressionNoIn :                                               See 11.14
--         AssignmentExpressionNoIn
--         ExpressionNoIn , AssignmentExpressionNoIn
ExpressionNoIn :: { AST.JSNode }
ExpressionNoIn : AssignmentExpressionNoIn { fp (AST.NS (AST.JSExpression $1) (mex $1) []) {- Expression -} }
               | ExpressionNoIn ',' AssignmentExpressionNoIn  { flattenExpression $1 (mpc $3 (gc $2)) }

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
-- StatementBlock : '{' '}'               { (AST.NS (AST.JSLiteral ";") (ss $1) []) }
--                | '{' StatementList '}' { (if ($2 == AST.JSStatementList [AST.JSLiteral ";"])
--                                             then (AST.NS (AST.JSLiteral ";") (ss $1) [])
--                                             else (AST.NS (AST.JSBlock $2   ) (ss $1) []))
--                                        }
StatementBlock :: { AST.JSNode }
StatementBlock : '{' '}'               { fp (AST.NS (AST.JSStatementBlock (AST.NS (AST.JSStatementList []) (ss $1) [])) (ss $1) (mgc [$1,$2])) }
               | '{' StatementList '}' { fp (AST.NS (AST.JSStatementBlock $2) (ss $1) (mgc [$1,$3])) }

-- Block :                                                        See 12.1
--         { StatementListopt }
Block :: { AST.JSNode }
Block : '{' '}'               { fp (AST.NS (AST.JSBlock (AST.NS (AST.JSStatementList []) (ss $1) [])) (ss $1) (mgc [$1,$2])) }
      | '{' StatementList '}' { fp (AST.NS (AST.JSBlock $2) (ss $1) (mgc [$1,$3])) }

-- StatementList :                                                See 12.1
--         Statement
--         StatementList Statement
StatementList :: { AST.JSNode }
StatementList : Statement               { fp (AST.NS (AST.JSStatementList [$1]) (ex $1) []) }
              | StatementList Statement { (combineStatements $1 $2) }

-- VariableStatement :                                            See 12.2
--         var VariableDeclarationList ;
VariableStatement :: { AST.JSNode }
VariableStatement : 'var'   VariableDeclarationList AutoSemi { fp (AST.NS (AST.JSVariables "var" $2) (ss $1) (gc $1))}
                  | 'const' VariableDeclarationList AutoSemi { fp (AST.NS (AST.JSVariables "const" $2) (ss $1) (gc $1))}

-- VariableDeclarationList :                                      See 12.2
--         VariableDeclaration
--         VariableDeclarationList , VariableDeclaration
VariableDeclarationList :: { [AST.JSNode] }
VariableDeclarationList : VariableDeclaration { [$1] {- VariableDeclarationList -}}
                        | VariableDeclarationList ',' VariableDeclaration { ($1 ++ [(pc $3 (gc $2))]) {- VariableDeclarationList -}}

-- VariableDeclarationListNoIn :                                  See 12.2
--         VariableDeclarationNoIn
--         VariableDeclarationListNoIn , VariableDeclarationNoIn
VariableDeclarationListNoIn :: { [AST.JSNode] }
VariableDeclarationListNoIn : VariableDeclarationNoIn { [$1] {- VariableDeclarationList -}}
                            | VariableDeclarationListNoIn ',' VariableDeclarationNoIn { ($1 ++ [(pc $3 (gc $2))]) {- VariableDeclarationList -}}

-- VariableDeclaration :                                          See 12.2
--         Identifier Initialiseropt
VariableDeclaration :: { AST.JSNode }
VariableDeclaration : Identifier              { fp (AST.NS (AST.JSVarDecl $1 []) (ex $1) [])}
                    | Identifier Initializer  { fp (AST.NS (AST.JSVarDecl $1 $2) (ex $1) [])}

-- VariableDeclarationNoIn :                                      See 12.2
--         Identifier InitialiserNoInopt
VariableDeclarationNoIn :: { AST.JSNode }
VariableDeclarationNoIn : Identifier InitializerNoIn { fp (AST.NS (AST.JSVarDecl $1 $2) (ex $1) []) }
                        | Identifier                 { fp (AST.NS (AST.JSVarDecl $1 []) (ex $1) []) }

-- Initialiser :                                                                            See 12.2
--         = AssignmentExpression
Initializer :: { [AST.JSNode] }
Initializer : '=' AssignmentExpression { (mpc $2 (gc $1)) {- Initializer -} }

-- InitialiserNoIn :                                                                        See 12.2
--         = AssignmentExpressionNoIn
InitializerNoIn :: { [AST.JSNode] }
InitializerNoIn : '=' AssignmentExpressionNoIn { (mpc $2 (gc $1)) {- InitializerNoIn -}}

-- EmptyStatement :                                                                         See 12.3
--         ;
EmptyStatement :: { AST.JSNode }
EmptyStatement : ';' { fp (AST.NS (AST.JSLiteral ";") (ss $1) (gc $1)) }

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
IfStatement :: { AST.JSNode } -- +++XXXX++
IfStatement : 'if' '(' Expression ')' StatementSemi  IfElseRest
                  { (if ($6 /= []) then
                       (if (length $6 == 1)
                        then
                          (fp (AST.NS (AST.JSIfElse $3 $5 (head $6)) (ss $1) (mgc [$1,$2,$4]) ))
                        else
                          (fp (AST.NS
                             (AST.JSIfElse
                                $3
                                (fp (AST.NS
                                   (AST.JSBlock (AST.NS (AST.JSStatementList [$5]) (ss $1) []) )
                                   (ss $1) []
                                   ))
                                (last $6)
                             )
                             (ss $1) (mgc [$1,$2,$4])
                           ))
                       )
                     else (fp (AST.NS (AST.JSIf $3 $5) (ss $1) (mgc [$1,$2,$4]))) ) }

IfElseRest :: { [AST.JSNode] }
IfElseRest : 'else' Statement     { [(pc $2 (gc $1))] }
           |                      { [] }

StatementSemi :: { AST.JSNode }
StatementSemi : StatementNoEmpty ';' { fp (AST.NS (AST.JSBlock (AST.NS (AST.JSStatementList [$1]) (ex $1) [])) (ex $1) (gc $2)) }
              | StatementNoEmpty     { $1 {- StatementSemi -}}
              | ';'                  { fp (AST.NS (AST.JSLiteral ";") (ss $1) (gc $1)) }


-- IterationStatement :                                                                     See 12.6
--         do Statement while ( Expression );
--         while ( Expression ) Statement
--         for (ExpressionNoInopt; Expressionopt ; Expressionopt ) Statement
--         for ( var VariableDeclarationListNoIn; Expressionopt ; Expressionopt ) Statement
--         for ( LeftHandSideExpression in Expression ) Statement
--         for ( var VariableDeclarationNoIn in Expression ) Statement
IterationStatement :: { AST.JSNode }
IterationStatement : 'do' Statement 'while' '(' Expression ')' AutoSemi
                     { fp (AST.NS (AST.JSDoWhile $2 $5 $7) (ss $1) (mgc [$1,$3,$4,$6])) }
                   | 'while' '(' Expression ')' Statement
                     { fp (AST.NS (AST.JSWhile $3 $5) (ss $1) (mgc [$1,$2,$4])) }
                   | 'for' '(' ExpressionNoInOpt ';' ExpressionOpt ';' ExpressionOpt ')' Statement
                     { fp (AST.NS (AST.JSFor $3 $5 $7 $9) (ss $1) (mgc [$1,$2,$4,$6,$8])) }
                   | 'for' '(' 'var' VariableDeclarationListNoIn ';' ExpressionOpt ';' ExpressionOpt ')' Statement
                     { fp (AST.NS (AST.JSForVar $4 $6 $8 $10) (ss $1) (mgc [$1,$2,$3,$5,$7,$9])) }
                   | 'for' '(' LeftHandSideExpression 'in' Expression ')' Statement
                     { fp (AST.NS (AST.JSForIn $3 $5 $7) (ss $1) (mgc [$1,$2,$4,$6])) }
                   | 'for' '(' 'var' VariableDeclarationNoIn 'in' Expression ')' Statement
                     { fp (AST.NS (AST.JSForVarIn $4 $6 $8) (ss $1) (mgc [$1,$2,$3,$5,$7])) }

-- ContinueStatement :                                                                      See 12.7
--         continue [no LineTerminator here] Identifieropt ;
-- TODO: deal with [no LineTerminator here]
ContinueStatement :: { AST.JSNode }
ContinueStatement : 'continue' AutoSemi             { fp (AST.NS (AST.JSContinue [$2])    (ss $1) (gc $1)) }
                  | 'continue' Identifier AutoSemi  { fp (AST.NS (AST.JSContinue [$2,$3]) (ss $1) (gc $1)) }

-- BreakStatement :                                                                         See 12.8
--         break [no LineTerminator here] Identifieropt ;
-- TODO: deal with [no LineTerminator here]
BreakStatement :: { AST.JSNode }
BreakStatement : 'break' AutoSemi             { fp (AST.NS (AST.JSBreak []   [$2]) (ss $1) (gc $1)) }
               | 'break' Identifier AutoSemi  { fp (AST.NS (AST.JSBreak [$2] [$3]) (ss $1) (gc $1)) }

-- ReturnStatement :                                                                        See 12.9
--         return [no LineTerminator here] Expressionopt ;
-- TODO: deal with [no LineTerminator here]
ReturnStatement :: { AST.JSNode }
ReturnStatement : 'return' AutoSemi             { fp (AST.NS (AST.JSReturn [$2])    (ss $1) (gc $1)) }
                | 'return' Expression AutoSemi  { fp (AST.NS (AST.JSReturn [$2,$3]) (ss $1) (gc $1)) }

-- WithStatement :                                                                          See 12.10
--         with ( Expression ) Statement
WithStatement :: { AST.JSNode }
WithStatement : 'with' '(' Expression ')' Statement AutoSemi  { fp (AST.NS (AST.JSWith $3 [$5,$6]) (ss $1) (mgc [$1,$2,$4])) }

-- SwitchStatement :                                                                        See 12.11
--         switch ( Expression ) CaseBlock
SwitchStatement :: { AST.JSNode }
SwitchStatement : 'switch' '(' Expression ')' CaseBlock { (AST.NS (AST.JSSwitch $3 $5) (ss $1) (mgc [$1,$2,$4])) }

-- CaseBlock :                                                                              See 12.11
--         { CaseClausesopt }
--         { CaseClausesopt DefaultClause CaseClausesopt }
CaseBlock :: { [AST.JSNode] }
CaseBlock : '{' CaseClausesOpt '}'                              { (mpc $2 (mgc [$1,$3])       )     {- CaseBlock1 -}}
          | '{' CaseClausesOpt DefaultClause CaseClausesOpt '}' { (mpc ($2++($3:$4)) (mgc [$1,$5])) {- CaseBlock2 -}}

-- CaseClauses :                                                                            See 12.11
--         CaseClause
--         CaseClauses CaseClause
CaseClausesOpt :: { [AST.JSNode] }
CaseClausesOpt : CaseClause                { [$1] {- CaseClauses1 -}}
               | CaseClausesOpt CaseClause { ($1++[$2]) {- CaseClauses2 -}}
               |                           { [fp (AST.NS (AST.JSLiteral "") tokenPosnEmpty []) ] } -- { [] }

-- CaseClause :                                                               See 12.11
--        case Expression : StatementListopt
CaseClause :: { AST.JSNode }
CaseClause : 'case' Expression ':' StatementList  { fp (AST.NS (AST.JSCase $2 $4) (ss $1) (mgc [$1,$3])) }
           | 'case' Expression ':'                { fp (AST.NS (AST.JSCase $2 (AST.NS (AST.JSStatementList []) (ss $1) [])) (ss $1) (mgc [$1,$3])) }

-- DefaultClause :                                                            See 12.11
--        default : StatementListopt
DefaultClause :: { AST.JSNode }
DefaultClause : 'default' ':'                { fp (AST.NS (AST.JSDefault (AST.NS (AST.JSStatementList []) (ss $1) [])) (ss $1) (mgc [$1,$2])) }
              | 'default' ':' StatementList  { fp (AST.NS (AST.JSDefault $3) (ss $1) (mgc [$1,$2])) }

-- LabelledStatement :                                                        See 12.12
--        Identifier : Statement
LabelledStatement :: { AST.JSNode }
LabelledStatement : Identifier ':' Statement { fp (AST.NS (AST.JSLabelled $1 $3) (ex $1) (gc $2)) }

-- ThrowStatement :                                                           See 12.13
--        throw [no LineTerminator here] Expression ;
-- TODO : sort out no LineTerminator here
--        Does it need a semi at the end?
ThrowStatement :: { AST.JSNode }
ThrowStatement : 'throw' Expression { fp (AST.NS (AST.JSThrow $2) (ss $1) (gc $1)) }

-- Note: worked in updated syntax as per https://developer.mozilla.org/en/JavaScript/Reference/Statements/try...catch
--   i.e., 0 or more catches, then an optional finally
-- TryStatement :                                                             See 12.14
--        try Block Catch
--        try Block Finally
--        try Block Catch Finally
TryStatement :: { AST.JSNode }
TryStatement : 'try' Block Catches         { fp (AST.NS (AST.JSTry $2 $3) (ss $1) (gc $1))         {- TryStatement1 -} }
             | 'try' Block Finally         { fp (AST.NS (AST.JSTry $2 [$3]) (ss $1) (gc $1))       {- TryStatement2 -} }
             | 'try' Block Catches Finally { fp (AST.NS (AST.JSTry $2 ($3++[$4])) (ss $1) (gc $1)) {- TryStatement3 -} }

Catches :: { [AST.JSNode] }
Catches : Catch         { [$1]       {- Catches 1 -} }
        | Catches Catch { ($1++[$2]) {- Catches 2 -} }

-- Note: worked in updated syntax as per https://developer.mozilla.org/en/JavaScript/Reference/Statements/try...catch
-- <Catch> ::= 'catch' '(' Identifier ')' <Block>
--   becomes
-- <Catch> ::= 'catch' '(' Identifier ')' <Block>
--           | 'catch' '(' Identifier 'if' ConditionalExpression ')' <Block>
Catch :: { AST.JSNode }
Catch : 'catch' '(' Identifier ')' Block                 { fp (AST.NS (AST.JSCatch $3 [] $5) (ss $1) (mgc [$1,$2,$4])) }
      | 'catch' '(' Identifier 'if' ConditionalExpression ')' Block  { fp (AST.NS (AST.JSCatch $3 $5 $7) (ss $1) (mgc [$1,$2,$4,$6])) }

-- Finally :                                                                  See 12.14
--        finally Block
Finally :: { AST.JSNode }
Finally : 'finally' Block { fp (AST.NS (AST.JSFinally $2) (ss $1) (gc $1)) }

-- DebuggerStatement :                                                        See 12.15
--        debugger ;
DebuggerStatement :: { AST.JSNode }
DebuggerStatement : 'debugger' AutoSemi { fp (AST.NS (AST.JSLiteral "debugger") (ss $1) (gc $1)) }

-- FunctionDeclaration :                                                      See clause 13
--        function Identifier ( FormalParameterListopt ) { FunctionBody }
FunctionDeclaration :: { AST.JSNode }
FunctionDeclaration : 'function' Identifier '(' FormalParameterList ')' '{' FunctionBody '}'
                      { fp (AST.NS (AST.JSFunction $2 $4 $7) (ss $1) (mgc[$1,$3,$5,$6,$8])) }
                    | 'function' Identifier '(' ')' '{' FunctionBody '}'
                      { fp (AST.NS (AST.JSFunction $2 [] $6) (ss $1) (mgc [$1,$3,$4,$5,$7])) }

-- FunctionExpression :                                                       See clause 13
--        function Identifieropt ( FormalParameterListopt ) { FunctionBody }
FunctionExpression :: { AST.JSNode }
FunctionExpression : 'function' IdentifierOpt '(' ')' '{' FunctionBody '}'
                     { fp (AST.NS (AST.JSFunctionExpression $2 [] $6) (ss $1) (mgc [$1,$3,$4,$5,$7])) }
                   | 'function' IdentifierOpt '(' FormalParameterList ')' '{' FunctionBody '}'
                     { fp (AST.NS (AST.JSFunctionExpression $2 $4 $7) (ss $1) (mgc [$1,$3,$5,$6,$8])) }

IdentifierOpt :: { [AST.JSNode] }
IdentifierOpt : Identifier { [$1] {- IdentifierOpt -}}
              |            { []   {- IdentifierOpt -}}

-- FormalParameterList :                                                      See clause 13
--        Identifier
--        FormalParameterList , Identifier
FormalParameterList :: { [AST.JSNode] }
FormalParameterList : Identifier                          { [$1] {- FormalParameterList -}}
                    | FormalParameterList ',' Identifier  { ($1++[(pc $3 (gc $2))]) }

-- FunctionBody :                                                             See clause 13
--        SourceElementsopt
FunctionBody :: { AST.JSNode }
FunctionBody : SourceElements { (AST.NS (AST.JSFunctionBody [$1]) (ex $1) []) }
             |                { (AST.NS (AST.JSFunctionBody [])   tokenPosnEmpty []) }

-- Program :                                                                  See clause 14
--        SourceElementsopt

Program :: { AST.JSNode }
Program : SourceElementsTop { $1 {- Program -}}

-- SourceElements :                                                           See clause 14
--        SourceElement
--        SourceElements SourceElement
SourceElements :: { AST.JSNode }
SourceElements : SourceElement                { fp (AST.NS (AST.JSSourceElements [$1]) (ex $1) []) }
               | SourceElements SourceElement { (combineSourceElements $1 $2) }

SourceElementsTop :: { AST.JSNode }
SourceElementsTop : SourceElement                   { fp (AST.NS (AST.JSSourceElementsTop [$1]) (ex $1) []) }
                  | SourceElementsTop SourceElement { (combineSourceElementsTop $1 $2) }

-- SourceElement :
--       Statement
--       FunctionDeclaration
SourceElement :: { AST.JSNode }
SourceElement : Statement            { $1 {- SourceElement1 -} }
              | FunctionDeclaration  { $1 {- SourceElement2 -} }

{

combineSourceElements :: AST.JSNode -> AST.JSNode -> AST.JSNode
combineSourceElements (AST.NS (AST.JSSourceElements xs) s1 c1) x1@(AST.NS x s2 c2) = fp (AST.NS (AST.JSSourceElements (xs++[x1])) s1 c1)

combineSourceElementsTop :: AST.JSNode -> AST.JSNode -> AST.JSNode
combineSourceElementsTop (AST.NS (AST.JSSourceElementsTop xs) s1 c1) x1@(AST.NS x s2 c2) = fp (AST.NS (AST.JSSourceElementsTop (xs++[x1])) s1 c1)

combineStatements :: AST.JSNode -> AST.JSNode -> AST.JSNode
combineStatements (AST.NS (AST.JSStatementList xs) s1 c1) (AST.NS (AST.JSStatementList ys) s2 c2) = fp (AST.NS (AST.JSStatementList (xs++ys) ) s1 c2)
combineStatements (AST.NS (AST.JSStatementList xs) s1 c1) y = fp (AST.NS (AST.JSStatementList (xs++[y])) s1 c1)

parseError :: Token -> Alex a
-- parseError = throwError . UnexpectedToken
parseError tok = alexError (show tok)

flattenExpression :: AST.JSNode -> [AST.JSNode] -> AST.JSNode
flattenExpression (AST.NS (AST.JSExpression xs) s1 c1) e = fp (AST.NS (AST.JSExpression (xs++litComma++e)) s1 c1)
                        where
                          litComma :: [AST.JSNode]
                          litComma = [AST.NS (AST.JSLiteral ",") tokenPosnEmpty []]

-- --------------------------------

mex :: [AST.JSNode] -> TokenPosn
mex [] = tokenPosnEmpty
mex xs = ex (head xs)

ex :: AST.JSNode -> TokenPosn
ex (AST.NS _node span _c) = span

--ss token = toSrcSpan (token_span token)
ss :: Token -> TokenPosn
ss token = token_span token

-- ------------------------------

gc :: Token -> [CommentAnnotation]
gc token = token_comment token
mgc :: [Token] -> [CommentAnnotation]
mgc xs = concatMap gc xs

-- Get node comment
gnc   (AST.NS _ _ ca)  = ca
mgnc [(AST.NS _ _ ca)] = ca

-- Prepend a comment
pc :: AST.JSNode -> [CommentAnnotation] -> AST.JSNode
pc (AST.NS node span cs2) cs1 = (AST.NS node span (cs1++cs2))

mpc :: [AST.JSNode] -> [CommentAnnotation] -> [AST.JSNode]
-- mpc [(AST.NS node span cs2)]    cs1 = [(AST.NS node span (cs1++cs2))]
mpc ((AST.NS node span cs2):xs) cs1 =  (AST.NS node span (cs1++cs2)):xs

-- ---------------------------------------------------------------------

fp :: AST.JSNode -> AST.JSNode
fp (AST.NS x p cs) = (AST.NS x p' cs)
  where
    p' = case (filter (/= NoComment) cs) of
      [] -> p
      ((CommentA posn _):_)  -> posn

}

-- Set emacs mode
-- Local Variables:
-- mode:haskell
-- End:
