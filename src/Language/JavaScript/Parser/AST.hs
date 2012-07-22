{-# LANGUAGE DeriveDataTypeable #-}
module Language.JavaScript.Parser.AST
    ( JSNode (..)
    , JSAnnot (..)
    , JSBinOp (..)
    , JSUnaryOp (..)
    , JSSemi (..)
    , JSAssignOp (..)
    , JSTryCatch (..)
    , JSTryFinally (..)
    , JSStatement (..)
    , JSBlock (..)
    , JSSwitchParts (..)
    , JSAST (..)
    , showStripped
    ) where

import Data.List
import Language.JavaScript.Parser.SrcLocation (TokenPosn (..))
import Language.JavaScript.Parser.Token

-- ---------------------------------------------------------------------

data JSAnnot = JSAnnot TokenPosn [CommentAnnotation]-- ^Annotation: position and comment/whitespace information
             | JSNoAnnot -- ^No annotation
    deriving (Eq)

instance Show JSAnnot where
    show JSNoAnnot = "NoAnnot"
    show (JSAnnot pos cs) = "Annot (" ++ show pos ++ ") " ++ show cs

data JSBinOp
    = JSBinOpAnd JSAnnot
    | JSBinOpBitAnd JSAnnot
    | JSBinOpBitOr JSAnnot
    | JSBinOpBitXor JSAnnot
    | JSBinOpDivide JSAnnot
    | JSBinOpEq JSAnnot
    | JSBinOpGe JSAnnot
    | JSBinOpGt JSAnnot
    | JSBinOpIn JSAnnot
    | JSBinOpInstanceOf JSAnnot
    | JSBinOpLe JSAnnot
    | JSBinOpLsh JSAnnot
    | JSBinOpLt JSAnnot
    | JSBinOpMinus JSAnnot
    | JSBinOpMod JSAnnot
    | JSBinOpNeq JSAnnot
    | JSBinOpOr JSAnnot
    | JSBinOpPlus JSAnnot
    | JSBinOpRsh JSAnnot
    | JSBinOpStrictEq JSAnnot
    | JSBinOpStrictNeq JSAnnot
    | JSBinOpTimes JSAnnot
    | JSBinOpUrsh JSAnnot
    deriving (Show, Eq)

data JSUnaryOp
    = JSUnaryOpDecr JSAnnot
    | JSUnaryOpDelete JSAnnot
    | JSUnaryOpIncr JSAnnot
    | JSUnaryOpMinus JSAnnot
    | JSUnaryOpNot JSAnnot
    | JSUnaryOpPlus JSAnnot
    | JSUnaryOpTilde JSAnnot
    | JSUnaryOpTypeof JSAnnot
    | JSUnaryOpVoid JSAnnot
    deriving (Show, Eq)

data JSSemi
    = JSSemi JSAnnot
    | JSSemiAuto
    deriving (Show, Eq)

data JSAssignOp
    = JSAssign JSAnnot
    | JSTimesAssign JSAnnot
    | JSDivideAssign JSAnnot
    | JSModAssign JSAnnot
    | JSPlusAssign JSAnnot
    | JSMinusAssign JSAnnot
    | JSLshAssign JSAnnot
    | JSRshAssign JSAnnot
    | JSUrshAssign JSAnnot
    | JSBwAndAssign JSAnnot
    | JSBwXorAssign JSAnnot
    | JSBwOrAssign JSAnnot
    deriving (Show, Eq)

data JSTryCatch
    = JSCatch JSAnnot JSAnnot JSNode [JSNode] JSAnnot JSBlock -- ^ catch,lb,ident,[if,expr],rb,block
    deriving (Show, Eq)

data JSTryFinally
    = JSFinally JSAnnot JSBlock -- ^block
    | JSNoFinally
    deriving (Show, Eq)

data JSBlock
    = JSBlock JSAnnot [JSStatement] JSAnnot
    deriving (Show, Eq)

data JSSwitchParts
    = JSCase JSAnnot JSNode JSNode [JSStatement]    -- ^expr,colon,stmtlist
    | JSDefault JSAnnot JSNode [JSStatement] -- ^colon,stmtlist
    deriving (Show, Eq)

data JSStatement
    = JSStatementBlock JSBlock      -- ^statement block
    | JSBreak JSAnnot [JSNode] JSSemi        -- ^optional identifier, autosemi
    | JSConstant JSAnnot [JSStatement] JSSemi -- ^const, decl, autosemi
    | JSContinue JSAnnot [JSNode] JSSemi     -- ^optional identifier,autosemi
    | JSDoWhile JSAnnot JSStatement JSAnnot JSAnnot JSNode JSAnnot JSSemi -- ^do,stmt,while,lb,expr,rb,autosemi
    | JSFor JSAnnot JSAnnot [JSNode] JSNode [JSNode] JSNode [JSNode] JSAnnot JSStatement -- ^for,lb,expr,semi,expr,semi,expr,rb.stmt
    | JSForIn JSAnnot JSAnnot JSNode JSBinOp JSNode JSAnnot JSStatement -- ^for,lb,expr,in,expr,rb,stmt
    | JSForVar JSAnnot JSAnnot JSNode [JSStatement] JSNode [JSNode] JSNode [JSNode] JSAnnot JSStatement -- ^for,lb,var,vardecl,semi,expr,semi,expr,rb,stmt
    | JSForVarIn JSAnnot JSAnnot JSNode JSStatement JSBinOp JSNode JSAnnot JSStatement -- ^for,lb,var,vardecl,in,expr,rb,stmt
    | JSFunction JSAnnot JSNode JSAnnot [JSNode] JSAnnot JSBlock  -- ^fn,name, lb,parameter list,rb,block
    | JSIf JSAnnot JSAnnot JSNode JSAnnot [JSStatement] [JSStatement] -- ^if,(,expr,),stmt,optional rest
    | JSLabelled JSNode JSNode JSStatement -- ^identifier,colon,stmt
    | JSExpressionStatement JSNode
    | JSReturn JSAnnot [JSNode] JSSemi -- ^optional expression,autosemi
    | JSSwitch JSAnnot JSAnnot JSNode JSAnnot JSAnnot [JSSwitchParts] JSAnnot-- ^switch,lb,expr,rb,caseblock
    | JSThrow JSAnnot JSNode -- ^throw val
    | JSTry JSAnnot JSBlock [JSTryCatch] JSTryFinally -- ^try,block,catches,finally
    | JSVarDecl JSNode -- ^identifier
    | JSVarDeclInit JSNode JSNode JSNode -- ^identifier, assignop, initializer
    | JSVariable JSAnnot [JSStatement] JSSemi -- ^var|const, decl, autosemi
    | JSWhile JSAnnot JSAnnot JSNode JSAnnot JSStatement -- ^while,lb,expr,rb,stmt
    | JSWith JSAnnot JSAnnot JSNode JSAnnot JSStatement JSSemi -- ^with,lb,expr,rb,stmt list
    deriving (Show, Eq)

data JSAST
    = JSSourceElementsTop [JSStatement] -- ^source elements
    deriving (Show, Eq)

-- | The JSNode is the building block of the AST.
-- Each has a syntactic part 'Node'. In addition, the leaf elements
-- (terminals) have a position 'TokenPosn', as well as an array of comments
-- and/or whitespace that was collected while parsing.

data JSNode
    -- | Terminals
    = JSIdentifier JSAnnot String
    | JSDecimal JSAnnot String
    | JSLiteral JSAnnot String
    | JSHexInteger JSAnnot String
    | JSOctal JSAnnot String
    | JSStringLiteral JSAnnot Char String
    | JSRegEx JSAnnot String

    -- | Non Terminals
    | JSArguments JSAnnot [JSNode] JSAnnot    -- ^lb, args, rb
    | JSArrayLiteral JSAnnot [JSNode] JSAnnot -- ^lb, contents, rb
    | JSAssignExpression JSNode JSAssignOp JSNode -- ^lhs, assignop, rhs
    | JSCallExpression [JSNode] [JSNode] [JSNode]  -- ^type : ., (), []; opening [ or ., contents, closing
    | JSCallExpressionDot JSNode [JSNode]  -- ^type : ., (), []; opening [ or ., contents, closing
    | JSCallExpressionSquare JSAnnot [JSNode] JSAnnot  -- ^type : ., (), []; opening [ or ., contents, closing
    | JSElision JSNode               -- ^comma
    | JSExpression [JSNode]          -- ^expression components
    | JSExpressionBinary JSNode JSBinOp JSNode -- ^lhs, op, rhs
    | JSExpressionParen JSAnnot JSNode JSAnnot -- ^lb,expression,rb
    | JSExpressionPostfix JSNode JSUnaryOp -- ^expression, operator
    | JSExpressionTernary JSNode JSNode JSNode JSNode JSNode -- ^cond, ?, trueval, :, falseval
    | JSFunctionExpression JSAnnot [JSNode] JSAnnot [JSNode] JSAnnot JSBlock -- ^fn,[name],lb, parameter list,rb,block`
    | JSMemberDot JSNode JSAnnot JSNode -- ^firstpart, dot, name
    | JSMemberSquare JSNode JSAnnot JSNode JSAnnot -- ^firstpart, lb, expr, rb
    | JSObjectLiteral JSAnnot [JSNode] JSAnnot -- ^lbrace contents rbrace
    | JSPropertyAccessor JSNode JSNode JSAnnot [JSNode] JSAnnot JSBlock -- ^(get|set), name, lb, params, rb, block
    | JSPropertyNameandValue JSNode JSNode [JSNode] -- ^name, colon, value
    | JSUnaryExpression JSUnaryOp JSNode
    deriving (Show, Eq)

-- Strip out the location info, leaving the original JSNode text representation
showStripped :: JSAST -> String
showStripped (JSSourceElementsTop xs) = "JSSourceElementsTop " ++ ssts xs


ss :: JSNode -> String
ss (JSArguments _lb xs _rb) = "JSArguments " ++ sss xs
ss (JSArrayLiteral _lb xs _rb) = "JSArrayLiteral " ++ sss xs
ss (JSAssignExpression lhs op rhs) = "JSExpression " ++ ss lhs ++ " " ++ sopa op ++ " " ++ ss rhs
ss (JSCallExpression _os xs _cs) = "JSCallExpression \"()\" " ++ sss xs
ss (JSCallExpressionDot _os xs) = "JSCallExpression \".\" " ++ sss xs
ss (JSCallExpressionSquare _os xs _cs) = "JSCallExpression \"[]\" " ++ sss xs
ss (JSDecimal _ s) = "JSDecimal " ++ show s
ss (JSElision c) = "JSElision " ++ ss c
ss (JSExpression xs) = "JSExpression " ++ sss xs
ss (JSExpressionBinary x2 op x3) = "JSExpressionBinary " ++ sbop op ++ " " ++ ss x2 ++ " " ++ ss x3
ss (JSExpressionParen _lp x _rp) = "JSExpressionParen (" ++ ss x ++ ")"
ss (JSExpressionPostfix xs op) = "JSExpressionPostfix " ++ suop op ++ " " ++ ss xs
ss (JSExpressionTernary x1 _q x2 _c x3) = "JSExpressionTernary " ++ ss x1 ++ " " ++ ss x2 ++ " " ++ ss x3
ss (JSFunctionExpression _ x1s _lb x2s _rb x3) = "JSFunctionExpression " ++ sss x1s ++ " " ++ sss x2s ++ " (" ++ ssb x3 ++ ")"
ss (JSHexInteger _ s) = "JSHexInteger " ++ show s
ss (JSOctal _ s) = "JSOctal " ++ show s
ss (JSIdentifier _ s) = "JSIdentifier " ++ show s
ss (JSLiteral _ []) = ""
ss (JSLiteral _ s) = "JSLiteral " ++ show s
ss (JSMemberDot x1s _d x2 ) = "JSMemberDot " ++ ss x1s ++ " (" ++ ss x2 ++ ")"
ss (JSMemberSquare x1s _lb x2 _rb) = "JSMemberSquare " ++ ss x1s ++ " (" ++ ss x2 ++ ")"
ss (JSObjectLiteral _lb xs _rb) = "JSObjectLiteral " ++ sss xs
ss (JSPropertyNameandValue x1 _colon x2s) = "JSPropertyNameandValue (" ++ ss x1 ++ ") " ++ sss x2s
ss (JSPropertyAccessor s x1 _lb1 x2s _rb1 x3) = "JSPropertyAccessor " ++ show s ++ " (" ++ ss x1 ++ ") " ++ sss x2s ++ " (" ++ ssb x3 ++ ")"
ss (JSRegEx _ s) = "JSRegEx " ++ show s
ss (JSStringLiteral _ c s) = "JSStringLiteral " ++ show c ++ " " ++ show s
ss (JSUnaryExpression op x) = "JSUnaryExpression " ++ suop op ++ ss x

sss :: [JSNode] -> String
sss xs = "[" ++ commaJoin (map ss xs) ++ "]"

-- The test suite expects operators to be double quoted.
sbop :: JSBinOp -> String
sbop = show . showbinop

showbinop :: JSBinOp -> String
showbinop (JSBinOpAnd _) = "&&"
showbinop (JSBinOpBitAnd _) = "&"
showbinop (JSBinOpBitOr _) = "|"
showbinop (JSBinOpBitXor _) = "^"
showbinop (JSBinOpDivide _) = "/"
showbinop (JSBinOpEq _) = "=="
showbinop (JSBinOpGe _) = ">="
showbinop (JSBinOpGt _) = ">"
showbinop (JSBinOpIn _) = " in "
showbinop (JSBinOpInstanceOf _) = "instanceof"
showbinop (JSBinOpLe _) = "<="
showbinop (JSBinOpLsh _) = "<<"
showbinop (JSBinOpLt _) = "<"
showbinop (JSBinOpMinus _) = "-"
showbinop (JSBinOpMod _) = "%"
showbinop (JSBinOpNeq _) = "!="
showbinop (JSBinOpOr _) = "||"
showbinop (JSBinOpPlus _) = "+"
showbinop (JSBinOpRsh _) = ">>"
showbinop (JSBinOpStrictEq _) = "==="
showbinop (JSBinOpStrictNeq _) = "!=="
showbinop (JSBinOpTimes _) = "*"
showbinop (JSBinOpUrsh _) = ">>>"

suop :: JSUnaryOp -> String
suop = show . showuop

showuop :: JSUnaryOp -> String
showuop (JSUnaryOpDecr _) = "--"
showuop (JSUnaryOpDelete _) = "delete "
showuop (JSUnaryOpIncr _) = "++"
showuop (JSUnaryOpMinus _) = "-"
showuop (JSUnaryOpNot _) = "!"
showuop (JSUnaryOpPlus _) = "+"
showuop (JSUnaryOpTilde _) = "~"
showuop (JSUnaryOpTypeof _) = "typeof "
showuop (JSUnaryOpVoid _) = "void "

showsemi :: JSSemi -> String
showsemi (JSSemi _) = "JSLiteral \";\""
showsemi JSSemiAuto = ""

sopa :: JSAssignOp -> String
sopa (JSAssign _) = "="
sopa (JSTimesAssign _) = "*="
sopa (JSDivideAssign _) = "/="
sopa (JSModAssign _) = "%="
sopa (JSPlusAssign _) = "+="
sopa (JSMinusAssign _) = "-="
sopa (JSLshAssign _) = "<<="
sopa (JSRshAssign _) = ">>="
sopa (JSUrshAssign _) = ">>>="
sopa (JSBwAndAssign _) = "&="
sopa (JSBwXorAssign _) = "^="
sopa (JSBwOrAssign _) = "|="

stcs :: [JSTryCatch] -> String
stcs xs = "[" ++ commaJoin (map stc xs) ++ "]"

stc :: JSTryCatch -> String
stc (JSCatch _ _lb x1 x2s _rb x3) = "JSCatch (" ++ ss x1 ++ ") " ++ sss x2s ++ " (" ++ ssb x3 ++ ")"

stf :: JSTryFinally -> String
stf (JSFinally _ x) = "JSFinally (" ++ ssb x ++ ")"
stf JSNoFinally = ""

sst :: JSStatement -> String
sst (JSStatementBlock blk) = "JSStatementBlock (" ++ ssb blk ++ ")"
sst (JSBreak _ x1s s) = "JSBreak " ++ sss x1s ++ " " ++ showsemi s
sst (JSContinue _ xs s) = "JSContinue " ++ sss xs ++ " " ++ showsemi s
sst (JSConstant _ xs _as) = "JSConstant const " ++ ssts xs
sst (JSDoWhile _d x1 _w _lb x2 _rb x3) = "JSDoWhile (" ++ sst x1 ++ ") (" ++ ss x2 ++ ") (" ++ showsemi x3 ++ ")"
sst (JSFor _ _lb x1s _s1 x2s _s2 x3s _rb x4) = "JSFor " ++ sss x1s ++ " " ++ sss x2s ++ " " ++ sss x3s ++ " (" ++ sst x4 ++ ")"
sst (JSForIn _ _lb x1s _i x2 _rb x3) = "JSForIn " ++ ss x1s ++ " (" ++ ss x2 ++ ") (" ++ sst x3 ++ ")"
sst (JSForVar _ _lb _v x1s _s1 x2s _s2 x3s _rb x4) = "JSForVar " ++ ssts x1s ++ " " ++ sss x2s ++ " " ++ sss x3s ++ " (" ++ sst x4 ++ ")"
sst (JSForVarIn _ _lb _v x1 _i x2 _rb x3) = "JSForVarIn (" ++ sst x1 ++ ") (" ++ ss x2 ++ ") (" ++ sst x3 ++ ")"
sst (JSFunction _ x1 _lb x2s _rb x3) = "JSFunction (" ++ ss x1 ++ ") " ++ sss x2s ++ " (" ++ ssb x3 ++ ")"
sst (JSIf _ _lb x1 _rb x2s x3s) = "JSIf (" ++ ss x1 ++ ") (" ++ ssts x2s ++ ") (" ++ ssts x3s ++ ")"
sst (JSLabelled x1 _c x2) = "JSLabelled (" ++ ss x1 ++ ") (" ++ sst x2 ++ ")"
sst (JSExpressionStatement l) = ss l
sst (JSReturn _ xs s) = "JSReturn " ++ sss xs ++ " " ++ showsemi s
sst (JSSwitch _ _lp x _rp _lb x2 _rb) = "JSSwitch (" ++ ss x ++ ") " ++ ssws x2
sst (JSThrow _ x) = "JSThrow (" ++ ss x ++ ")"
sst (JSTry _ xt1 xtc xtf) = "JSTry (" ++ ssb xt1 ++ ") " ++ stcs xtc ++ stf xtf
sst (JSVarDecl x1) = "JSVarDecl (" ++ ss x1 ++ ") "
sst (JSVarDeclInit x1 x2 x3) = "JSVarDecl (" ++ ss x1 ++ ") " ++ ss x2 ++ " " ++ ss x3
sst (JSVariable _ xs _as) = "JSVariable var " ++ ssts xs
sst (JSWhile _ _lb x1 _rb x2) = "JSWhile (" ++ ss x1 ++ ") (" ++ sst x2 ++ ")"
sst (JSWith _ _lb x1 _rb x s) = "JSWith (" ++ ss x1 ++ ") " ++ sst x ++ showsemi s

ssts :: [JSStatement] -> String
ssts xs = "[" ++ commaJoin (map sst xs) ++ "]"

ssb :: JSBlock -> String
ssb (JSBlock _ xs _) = "JSStatementBlock (" ++ ssts xs ++ ")"

ssw :: JSSwitchParts -> String
ssw (JSCase _ x1 _c x2s) = "JSCase (" ++ ss x1 ++ ") (" ++ ssts x2s ++ ")"
ssw (JSDefault _ _c xs) = "JSDefault (" ++ ssts xs ++ ")"

ssws :: [JSSwitchParts] -> String
ssws xs = "[" ++ commaJoin (map ssw xs) ++ "]"


commaJoin :: [String] -> String
commaJoin s = concat $ intersperse "," $ filter (not . null) s

-- EOF
