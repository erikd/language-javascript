{-# LANGUAGE DeriveDataTypeable #-}
module Language.JavaScript.Parser.AST
    ( JSNode (..)
    , JSAnnot (..)
    , JSBinOp (..)
    , JSUnaryOp (..)
    , JSLParen (..)
    , JSRParen (..)
    , JSLBrace (..)
    , JSRBrace (..)
    , JSLSquare (..)
    , JSRSquare (..)
    , JSSemi (..)
    , JSAssignOp (..)
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

data JSLParen
    = JSLParen JSAnnot
    deriving (Show, Eq)

data JSRParen
    = JSRParen JSAnnot
    deriving (Show, Eq)

data JSLBrace
    = JSLBrace JSAnnot
    deriving (Show, Eq)

data JSRBrace
    = JSRBrace JSAnnot
    deriving (Show, Eq)

data JSLSquare
    = JSLSquare JSAnnot
    deriving (Show, Eq)

data JSRSquare
    = JSRSquare JSAnnot
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
    | JSArguments JSLParen [JSNode] JSRParen    -- ^lb, args, rb
    | JSArrayLiteral JSLSquare [JSNode] JSRSquare -- ^lb, contents, rb
    | JSAssignExpression JSNode JSAssignOp JSNode -- ^lhs, assignop, rhs
    | JSBlock JSLBrace [JSNode] JSRBrace      -- ^lb,optional block statements,rb
    | JSBreak JSAnnot [JSNode] JSSemi        -- ^optional identifier, autosemi
    | JSCallExpression [JSNode] [JSNode] [JSNode]  -- ^type : ., (), []; opening [ or ., contents, closing
    | JSCallExpressionDot JSNode [JSNode]  -- ^type : ., (), []; opening [ or ., contents, closing
    | JSCallExpressionSquare JSLSquare [JSNode] JSRSquare  -- ^type : ., (), []; opening [ or ., contents, closing
    | JSCase JSAnnot JSNode JSNode [JSNode]    -- ^expr,colon,stmtlist
    | JSCatch JSAnnot JSLParen JSNode [JSNode] JSRParen JSNode -- ^ catch,lb,ident,[if,expr],rb,block
    | JSContinue JSAnnot [JSNode] JSSemi     -- ^optional identifier,autosemi
    | JSDefault JSAnnot JSNode [JSNode] -- ^colon,stmtlist
    | JSDoWhile JSAnnot JSNode JSAnnot JSLParen JSNode JSRParen JSSemi -- ^do,stmt,while,lb,expr,rb,autosemi
    | JSElision JSAnnot JSNode               -- ^comma
    | JSExpression [JSNode]          -- ^expression components
    | JSExpressionBinary JSNode JSBinOp JSNode -- ^lhs, op, rhs
    | JSExpressionParen JSLParen JSNode JSRParen -- ^lb,expression,rb
    | JSExpressionPostfix JSNode JSUnaryOp -- ^expression, operator
    | JSExpressionTernary JSNode JSNode JSNode JSNode JSNode -- ^cond, ?, trueval, :, falseval
    | JSFinally JSAnnot JSNode -- ^block
    | JSFor JSAnnot JSNode JSLParen [JSNode] JSNode [JSNode] JSNode [JSNode] JSRParen JSNode -- ^for,lb,expr,semi,expr,semi,expr,rb.stmt
    | JSForIn JSAnnot JSNode JSLParen JSNode JSBinOp JSNode JSRParen JSNode -- ^for,lb,expr,in,expr,rb,stmt
    | JSForVar JSAnnot JSNode JSLParen JSNode [JSNode] JSNode [JSNode] JSNode [JSNode] JSRParen JSNode -- ^for,lb,var,vardecl,semi,expr,semi,expr,rb,stmt
    | JSForVarIn JSAnnot JSNode JSLParen JSNode JSNode JSBinOp JSNode JSRParen JSNode -- ^for,lb,var,vardecl,in,expr,rb,stmt
    | JSFunction JSNode JSNode JSLParen [JSNode] JSRParen JSNode  -- ^fn,name, lb,parameter list,rb,block
    | JSFunctionExpression JSNode [JSNode] JSLParen [JSNode] JSRParen JSNode  -- ^fn,[name],lb, parameter list,rb,block`
    | JSIf JSNode JSLParen JSNode JSRParen [JSNode] [JSNode] -- ^if,(,expr,),stmt,optional rest
    | JSLabelled JSNode JSNode JSNode -- ^identifier,colon,stmt
    | JSMemberDot JSNode JSNode JSNode -- ^firstpart, dot, name
    | JSMemberSquare JSNode JSLSquare JSNode JSRSquare -- ^firstpart, lb, expr, rb
    | JSObjectLiteral JSLBrace [JSNode] JSRBrace -- ^lbrace contents rbrace
    | JSOpAssign JSAssignOp -- ^opnode
    | JSPropertyAccessor JSAnnot JSNode JSNode JSLParen [JSNode] JSRParen JSNode -- ^(get|set), name, lb, params, rb, block
    | JSPropertyNameandValue JSAnnot JSNode JSNode [JSNode] -- ^name, colon, value
    | JSReturn JSAnnot [JSNode] JSSemi -- ^optional expression,autosemi
    | JSSourceElementsTop JSAnnot [JSNode] -- ^source elements
    | JSSwitch JSAnnot JSLParen JSNode JSRParen JSNode -- ^switch,lb,expr,rb,caseblock
    | JSThrow JSAnnot JSNode -- ^throw val
    | JSTry JSAnnot JSNode [JSNode] -- ^try,block,rest
    | JSUnaryExpression JSUnaryOp JSNode
    | JSVarDecl JSAnnot JSNode [JSNode] -- ^identifier, optional initializer
    | JSVariables JSAnnot JSNode [JSNode] JSSemi -- ^var|const, decl, autosemi
    | JSWhile JSAnnot JSLParen JSNode JSRParen JSNode -- ^while,lb,expr,rb,stmt
    | JSWith JSAnnot JSLParen JSNode JSRParen JSNode JSSemi -- ^with,lb,expr,rb,stmt list
    deriving (Show, Eq)

-- Strip out the location info, leaving the original JSNode text representation
showStripped :: JSNode -> String
showStripped = ss

ss :: JSNode -> String
ss (JSArguments _lb xs _rb) = "JSArguments " ++ sss xs
ss (JSArrayLiteral _lb xs _rb) = "JSArrayLiteral " ++ sss xs
ss (JSAssignExpression lhs op rhs) = "JSExpression " ++ ss lhs ++ " " ++ sopa op ++ " " ++ ss rhs
ss (JSBlock _lb xs _rb) = "JSBlock (" ++ sss xs ++ ")"
ss (JSBreak _ x1s s) = "JSBreak " ++ sss x1s ++ " " ++ showsemi s
ss (JSCallExpression _os xs _cs) = "JSCallExpression \"()\" " ++ sss xs
ss (JSCallExpressionDot _os xs) = "JSCallExpression \".\" " ++ sss xs
ss (JSCallExpressionSquare _os xs _cs) = "JSCallExpression \"[]\" " ++ sss xs
ss (JSCase _ x1 _c x2s) = "JSCase (" ++ ss x1 ++ ") (" ++ sss x2s ++ ")"
ss (JSCatch _ _lb x1 x2s _rb x3) = "JSCatch (" ++ ss x1 ++ ") " ++ sss x2s ++ " (" ++ ss x3 ++ ")"
ss (JSContinue _ xs s) = "JSContinue " ++ sss xs ++ " " ++ showsemi s
ss (JSDecimal _ s) = "JSDecimal " ++ show s
ss (JSDefault _ _c xs) = "JSDefault (" ++ sss xs ++ ")"
ss (JSDoWhile _d x1 _w _lb x2 _rb x3) = "JSDoWhile (" ++ ss x1 ++ ") (" ++ ss x2 ++ ") (" ++ showsemi x3 ++ ")"
ss (JSElision _ c) = "JSElision " ++ ss c
ss (JSExpression xs) = "JSExpression " ++ sss xs
ss (JSExpressionBinary x2 op x3) = "JSExpressionBinary " ++ sbop op ++ " " ++ ss x2 ++ " " ++ ss x3
ss (JSExpressionParen _lp x _rp) = "JSExpressionParen (" ++ ss x ++ ")"
ss (JSExpressionPostfix xs op) = "JSExpressionPostfix " ++ suop op ++ " " ++ ss xs
ss (JSExpressionTernary x1 _q x2 _c x3) = "JSExpressionTernary " ++ ss x1 ++ " " ++ ss x2 ++ " " ++ ss x3
ss (JSFinally _ x) = "JSFinally (" ++ ss x ++ ")"
ss (JSFor _ _f _lb x1s _s1 x2s _s2 x3s _rb x4) = "JSFor " ++ sss x1s ++ " " ++ sss x2s ++ " " ++ sss x3s ++ " (" ++ ss x4 ++ ")"
ss (JSForIn _ _f _lb x1s _i x2 _rb x3) = "JSForIn " ++ ss x1s ++ " (" ++ ss x2 ++ ") (" ++ ss x3 ++ ")"
ss (JSForVar _ _f _lb _v x1s _s1 x2s _s2 x3s _rb x4) = "JSForVar " ++ sss x1s ++ " " ++ sss x2s ++ " " ++ sss x3s ++ " (" ++ ss x4 ++ ")"
ss (JSForVarIn _ _f _lb _v x1 _i x2 _rb x3) = "JSForVarIn (" ++ ss x1 ++ ") (" ++ ss x2 ++ ") (" ++ ss x3 ++ ")"
ss (JSFunction _f x1 _lb x2s _rb x3) = "JSFunction (" ++ ss x1 ++ ") " ++ sss x2s ++ " (" ++ ss x3 ++ ")"
ss (JSFunctionExpression _f x1s _lb x2s _rb x3) = "JSFunctionExpression " ++ sss x1s ++ " " ++ sss x2s ++ " (" ++ ss x3 ++ ")"
ss (JSHexInteger _ s) = "JSHexInteger " ++ show s
ss (JSOctal _ s) = "JSOctal " ++ show s
ss (JSIdentifier _ s) = "JSIdentifier " ++ show s
ss (JSIf _i _lb x1 _rb x2s x3s) = "JSIf (" ++ ss x1 ++ ") (" ++ sss x2s ++ ") (" ++ sss x3s ++ ")"
ss (JSLabelled x1 _c x2) = "JSLabelled (" ++ ss x1 ++ ") (" ++ ss x2 ++ ")"
ss (JSLiteral _ s) = "JSLiteral " ++ show s
ss (JSMemberDot x1s _d x2 ) = "JSMemberDot " ++ ss x1s ++ " (" ++ ss x2 ++ ")"
ss (JSMemberSquare x1s _lb x2 _rb) = "JSMemberSquare " ++ ss x1s ++ " (" ++ ss x2 ++ ")"
ss (JSObjectLiteral _lb xs _rb) = "JSObjectLiteral " ++ sss xs
ss (JSOpAssign n) = "JSOpAssign JSLiteral " ++ show (sopa n)
ss (JSPropertyNameandValue _ x1 _colon x2s) = "JSPropertyNameandValue (" ++ ss x1 ++ ") " ++ sss x2s
ss (JSPropertyAccessor _ s x1 _lb1 x2s _rb1 x3) = "JSPropertyAccessor " ++ show s ++ " (" ++ ss x1 ++ ") " ++ sss x2s ++ " (" ++ ss x3 ++ ")"
ss (JSRegEx _ s) = "JSRegEx " ++ show s
ss (JSReturn _ xs s) = "JSReturn " ++ sss xs ++ " " ++ showsemi s
ss (JSSourceElementsTop _ xs) = "JSSourceElementsTop " ++ sss xs
ss (JSStringLiteral _ c s) = "JSStringLiteral " ++ show c ++ " " ++ show s
ss (JSSwitch _ _lb x _rb x2) = "JSSwitch (" ++ ss x ++ ") " ++ ss x2
ss (JSThrow _ x) = "JSThrow (" ++ ss x ++ ")"
ss (JSTry _ x1 x2s) = "JSTry (" ++ ss x1 ++ ") " ++ sss x2s
ss (JSUnaryExpression op x) = "JSUnaryExpression " ++ suop op ++ ss x
ss (JSVarDecl _ x1 x2s) = "JSVarDecl (" ++ ss x1 ++ ") " ++ sss x2s
ss (JSVariables _ n xs _as) = "JSVariables " ++ ss n ++ " " ++ sss xs
ss (JSWhile _ _lb x1 _rb x2) = "JSWhile (" ++ ss x1 ++ ") (" ++ ss x2 ++ ")"
ss (JSWith _ _lb x1 _rb x s) = "JSWith (" ++ ss x1 ++ ") " ++ ssss [x] s

sss :: [JSNode] -> String
sss xs = "[" ++ (concat (intersperse "," $ map ss xs)) ++ "]"

ssss :: [JSNode] -> JSSemi -> String
ssss xs s@JSSemi{} = "[" ++ (concat (intersperse "," (map ss xs ++ [showsemi s]))) ++ "]"
ssss xs JSSemiAuto = sss xs

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


-- EOF
