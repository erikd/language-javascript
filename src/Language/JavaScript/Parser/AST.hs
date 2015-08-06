{-# LANGUAGE DeriveDataTypeable #-}

module Language.JavaScript.Parser.AST
    ( JSExpression (..)
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
    , JSAccessor (..)
    , JSIdent (..)
    , JSArguments (..)
    , JSVarInitializer (..)

    , JSList (..)
    , JSNonEmptyList (..)

    , showStripped
    ) where

import Data.Data
import Data.List
import Language.JavaScript.Parser.SrcLocation (TokenPosn (..))
import Language.JavaScript.Parser.Token

-- ---------------------------------------------------------------------

data JSAnnot
    = JSAnnot TokenPosn [CommentAnnotation] -- ^Annotation: position and comment/whitespace information
    | JSNoAnnot -- ^No annotation
    deriving (Data, Eq, Show, Typeable)


data JSAST
    = JSAstProgram [JSStatement] JSAnnot -- ^source elements, tailing whitespace
    | JSAstStatement JSStatement JSAnnot
    | JSAstExpression JSExpression JSAnnot
    | JSAstLiteral JSExpression JSAnnot
    deriving (Data, Eq, Show, Typeable)

data JSStatement
    = JSStatementBlock JSAnnot [JSStatement] JSAnnot JSSemi     -- ^lbrace, stmts, rbrace, autosemi
    | JSBreak JSAnnot JSIdent JSSemi        -- ^break,optional identifier, autosemi
    | JSConstant JSAnnot [JSExpression] JSSemi -- ^const, decl, autosemi
    | JSContinue JSAnnot JSIdent JSSemi     -- ^continue, optional identifier,autosemi
    | JSDoWhile JSAnnot JSStatement JSAnnot JSAnnot JSExpression JSAnnot JSSemi -- ^do,stmt,while,lb,expr,rb,autosemi
    | JSFor JSAnnot JSAnnot [JSExpression] JSAnnot [JSExpression] JSAnnot [JSExpression] JSAnnot JSStatement -- ^for,lb,expr,semi,expr,semi,expr,rb.stmt
    | JSForIn JSAnnot JSAnnot JSExpression JSBinOp JSExpression JSAnnot JSStatement -- ^for,lb,expr,in,expr,rb,stmt
    | JSForVar JSAnnot JSAnnot JSAnnot [JSExpression] JSAnnot [JSExpression] JSAnnot [JSExpression] JSAnnot JSStatement -- ^for,lb,var,vardecl,semi,expr,semi,expr,rb,stmt
    | JSForVarIn JSAnnot JSAnnot JSAnnot JSExpression JSBinOp JSExpression JSAnnot JSStatement -- ^for,lb,var,vardecl,in,expr,rb,stmt
    | JSFunction JSAnnot JSIdent JSAnnot (JSList JSIdent) JSAnnot JSBlock JSSemi  -- ^fn,name, lb,parameter list,rb,block,autosemi
    | JSIf JSAnnot JSAnnot JSExpression JSAnnot JSStatement -- ^if,(,expr,),stmt
    | JSIfElse JSAnnot JSAnnot JSExpression JSAnnot JSStatement JSAnnot JSStatement -- ^if,(,expr,),stmt,else,rest
    | JSLabelled JSExpression JSAnnot JSStatement -- ^identifier,colon,stmt
    | JSEmptyStatement JSAnnot
    | JSExpressionStatement JSExpression JSSemi
    | JSAssignStatement JSExpression JSAssignOp JSExpression JSSemi -- ^lhs, assignop, rhs, autosemi
    | JSMethodCall JSExpression JSArguments JSSemi
    | JSReturn JSAnnot (Maybe JSExpression) JSSemi -- ^optional expression,autosemi
    | JSSwitch JSAnnot JSAnnot JSExpression JSAnnot JSAnnot [JSSwitchParts] JSAnnot JSSemi -- ^switch,lb,expr,rb,caseblock,autosemi
    | JSThrow JSAnnot JSExpression JSSemi -- ^throw val autosemi
    | JSTry JSAnnot JSBlock [JSTryCatch] JSTryFinally -- ^try,block,catches,finally
    | JSVariable JSAnnot [JSExpression] JSSemi -- ^var|const, decl, autosemi
    | JSWhile JSAnnot JSAnnot JSExpression JSAnnot JSStatement -- ^while,lb,expr,rb,stmt
    | JSWith JSAnnot JSAnnot JSExpression JSAnnot JSStatement JSSemi -- ^with,lb,expr,rb,stmt list
    deriving (Data, Eq, Show, Typeable)

data JSExpression
    -- | Terminals
    = JSIdentifier JSAnnot String
    | JSDecimal JSAnnot String
    | JSLiteral JSAnnot String
    | JSHexInteger JSAnnot String
    | JSOctal JSAnnot String
    | JSStringLiteralS JSAnnot String
    | JSStringLiteralD JSAnnot String
    | JSRegEx JSAnnot String

    -- | Non Terminals
    | JSArrayLiteral JSAnnot [JSExpression] JSAnnot -- ^lb, contents, rb
    | JSAssignExpression JSExpression JSAssignOp JSExpression -- ^lhs, assignop, rhs
    | JSCallExpression JSExpression JSArguments  -- ^expr, args
    | JSCallExpressionDot JSExpression JSAnnot JSExpression  -- ^expr, dot, expr
    | JSCallExpressionSquare JSExpression JSAnnot JSExpression JSAnnot  -- ^expr, [, expr, ]
    | JSComma JSAnnot-- ^comma
    | JSCommaExpression JSExpression JSAnnot JSExpression          -- ^expression components
    | JSExpressionBinary JSExpression JSBinOp JSExpression -- ^lhs, op, rhs
    | JSExpressionParen JSAnnot JSExpression JSAnnot -- ^lb,expression,rb
    | JSExpressionPostfix JSExpression JSUnaryOp -- ^expression, operator
    | JSExpressionTernary JSExpression JSAnnot JSExpression JSAnnot JSExpression -- ^cond, ?, trueval, :, falseval
    | JSFunctionExpression JSAnnot JSIdent JSAnnot (JSList JSIdent) JSAnnot JSBlock -- ^fn,name,lb, parameter list,rb,block`
    | JSMemberDot JSExpression JSAnnot JSExpression -- ^firstpart, dot, name
    | JSMemberExpression JSExpression JSArguments -- expr, args
    | JSMemberNew JSAnnot JSExpression JSArguments -- ^new, name, args
    | JSMemberSquare JSExpression JSAnnot JSExpression JSAnnot -- ^firstpart, lb, expr, rb
    | JSNewExpression JSAnnot JSExpression -- ^new, expr
    | JSObjectLiteral JSAnnot [JSExpression] JSAnnot -- ^lbrace contents rbrace
    | JSPropertyAccessor JSAccessor JSIdent JSAnnot [JSExpression] JSAnnot JSBlock -- ^(get|set), name, lb, params, rb, block
    | JSPropertyNameandValue JSIdent JSAnnot [JSExpression] -- ^name, colon, value
    | JSUnaryExpression JSUnaryOp JSExpression
    | JSVarInitExpression JSExpression JSVarInitializer -- ^identifier, initializer
    deriving (Data, Eq, Show, Typeable)

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
    deriving (Data, Eq, Show, Typeable)

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
    deriving (Data, Eq, Show, Typeable)

data JSSemi
    = JSSemi JSAnnot
    | JSSemiAuto
    deriving (Data, Eq, Show, Typeable)

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
    deriving (Data, Eq, Show, Typeable)

data JSTryCatch
    = JSCatch JSAnnot JSAnnot JSExpression JSAnnot JSBlock -- ^catch,lb,ident,rb,block
    | JSCatchIf JSAnnot JSAnnot JSExpression JSAnnot JSExpression JSAnnot JSBlock -- ^catch,lb,ident,if,expr,rb,block
    deriving (Data, Eq, Show, Typeable)

data JSTryFinally
    = JSFinally JSAnnot JSBlock -- ^finally,block
    | JSNoFinally
    deriving (Data, Eq, Show, Typeable)

data JSBlock
    = JSBlock JSAnnot [JSStatement] JSAnnot -- ^lbrace, stmts, rbrace
    deriving (Data, Eq, Show, Typeable)

data JSSwitchParts
    = JSCase JSAnnot JSExpression JSAnnot [JSStatement]    -- ^expr,colon,stmtlist
    | JSDefault JSAnnot JSAnnot [JSStatement] -- ^colon,stmtlist
    deriving (Data, Eq, Show, Typeable)

data JSVarInitializer
    = JSVarInit JSAnnot JSExpression -- ^ assignop, initializer
    | JSVarInitNone
    deriving (Data, Eq, Show, Typeable)

-- | Accessors for JSPropertyAccessor is either 'get' or 'set'.
data JSAccessor
    = JSAccessorGet JSAnnot
    | JSAccessorSet JSAnnot
    deriving (Data, Eq, Show, Typeable)

data JSIdent
    = JSIdentName JSAnnot String
    | JSIdentNone
    deriving (Data, Eq, Show, Typeable)

data JSList a
    = JSParams (JSNonEmptyList a) -- ^tail, comma, ident
    | JSNoParams
    deriving (Data, Eq, Show, Typeable)

data JSNonEmptyList a
    = JSLCons (JSNonEmptyList a) JSAnnot a
    | JSLOne a
    deriving (Data, Eq, Show, Typeable)

data JSArguments
    = JSArguments JSAnnot (JSList JSExpression) JSAnnot    -- ^lb, args, rb
    deriving (Data, Eq, Show, Typeable)

-- Strip out the location info, leaving the original JSExpression text representation
showStripped :: JSAST -> String
showStripped (JSAstProgram xs _) = "JSAstProgram " ++ ssts xs
showStripped (JSAstStatement s _) = "JSAstStatement (" ++ sst s ++ ")"
showStripped (JSAstExpression e _) = "JSAstExpression (" ++ ss e ++ ")"
showStripped (JSAstLiteral s _)  = "JSAstLiteral (" ++ ss s ++ ")"


ss :: JSExpression -> String
ss (JSArrayLiteral _lb xs _rb) = "JSArrayLiteral " ++ sss xs
ss (JSAssignExpression lhs op rhs) = "JSOpAssign (" ++ sopa op ++ "," ++ ss lhs ++ "," ++ ss rhs ++ ")"
ss (JSCallExpression ex xs) = "JSCallExpression ("++ ss ex ++ "," ++ ssa xs ++ ")"
ss (JSCallExpressionDot ex _os xs) = "JSCallExpressionDot (" ++ ss ex ++ "," ++ ss xs ++ ")"
ss (JSCallExpressionSquare ex _os xs _cs) = "JSCallExpressionSquare (" ++ ss ex ++ "," ++ ss xs ++ ")"
ss (JSComma _) = "JSComma"
ss (JSDecimal _ s) = "JSDecimal " ++ singleQuote s
ss (JSCommaExpression l _ r) = "JSExpression [" ++ ss l ++ "," ++ ss r ++ "]"
ss (JSExpressionBinary x2 op x3) = "JSExpressionBinary (" ++ sbop op ++ "," ++ ss x2 ++ "," ++ ss x3 ++ ")"
ss (JSExpressionParen _lp x _rp) = "JSExpressionParen (" ++ ss x ++ ")"
ss (JSExpressionPostfix xs op) = "JSExpressionPostfix (" ++ suop op ++ "," ++ ss xs ++ ")"
ss (JSExpressionTernary x1 _q x2 _c x3) = "JSExpressionTernary (" ++ ss x1 ++ "," ++ ss x2 ++ "," ++ ss x3 ++ ")"
ss (JSFunctionExpression _ n _lb pl _rb x3) = "JSFunctionExpression " ++ ssid n ++ " " ++ ssjl pl ++ " (" ++ ssb x3 ++ "))"
ss (JSHexInteger _ s) = "JSHexInteger " ++ singleQuote s
ss (JSOctal _ s) = "JSOctal " ++ singleQuote s
ss (JSIdentifier _ s) = "JSIdentifier " ++ singleQuote s
ss (JSLiteral _ []) = "JSLiteral ''"
ss (JSLiteral _ s) = "JSLiteral " ++ singleQuote s
ss (JSMemberDot x1s _d x2 ) = "JSMemberDot (" ++ ss x1s ++ "," ++ ss x2 ++ ")"
ss (JSMemberExpression e a) = "JSMemberExpression (" ++ ss e ++ "," ++ ssa a ++ ")"
ss (JSMemberNew _a n s) = "JSMemberNew (" ++ ss n ++ "," ++ ssa s ++ ")"
ss (JSMemberSquare x1s _lb x2 _rb) = "JSMemberSquare (" ++ ss x1s ++ "," ++ ss x2 ++ ")"
ss (JSNewExpression _n e) = "JSNewExpression " ++ ss e
ss (JSObjectLiteral _lb xs _rb) = "JSObjectLiteral " ++ sss xs
ss (JSPropertyNameandValue x1 _colon x2s) = "JSPropertyNameandValue (" ++ ssid2 x1 ++ ") " ++ sss x2s
ss (JSPropertyAccessor s x1 _lb1 x2s _rb1 x3) = "JSPropertyAccessor " ++ ssac s ++ " (" ++ ssid2 x1 ++ ") " ++ sss x2s ++ " (" ++ ssb x3 ++ ")"
ss (JSRegEx _ s) = "JSRegEx " ++ singleQuote s
ss (JSStringLiteralS _ s) = "JSStringLiteralS " ++ singleQuote s
ss (JSStringLiteralD _ s) = "JSStringLiteralD " ++ singleQuote s
ss (JSUnaryExpression op x) = "JSUnaryExpression (" ++ suop op ++ "," ++ ss x ++ ")"
ss (JSVarInitExpression x1 x2) = "JSVarInitExpression (" ++ ss x1 ++ ") " ++ ssvi x2


singleQuote :: String -> String
singleQuote s = '\'' : (s ++ "'")

ssid :: JSIdent -> String
ssid (JSIdentName _ s) = singleQuote s
ssid JSIdentNone = "''"

ssid2 :: JSIdent -> String
ssid2 (JSIdentName _ s) = "JSIdentifier " ++ singleQuote s
ssid2 JSIdentNone = "JSIdentNone"

ssac :: JSAccessor -> String
ssac (JSAccessorGet _) = "JSAccessorGet"
ssac (JSAccessorSet _) = "JSAccessorSet"

sss :: [JSExpression] -> String
sss xs = "[" ++ commaJoin (map ss xs) ++ "]"

sbop :: JSBinOp -> String
sbop = singleQuote . showbinop

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
suop = singleQuote . showuop

showuop :: JSUnaryOp -> String
showuop (JSUnaryOpDecr _) = "--"
showuop (JSUnaryOpDelete _) = "delete"
showuop (JSUnaryOpIncr _) = "++"
showuop (JSUnaryOpMinus _) = "-"
showuop (JSUnaryOpNot _) = "!"
showuop (JSUnaryOpPlus _) = "+"
showuop (JSUnaryOpTilde _) = "~"
showuop (JSUnaryOpTypeof _) = "typeof"
showuop (JSUnaryOpVoid _) = "void"

showsemi :: JSSemi -> String
showsemi (JSSemi _) = "JSSemicolon"
showsemi JSSemiAuto = ""

commaIf :: String -> String
commaIf "" = ""
commaIf xs = ',' : xs

sopa :: JSAssignOp -> String
sopa = singleQuote . showOp

showOp :: JSAssignOp -> String
showOp (JSAssign _) = "="
showOp (JSTimesAssign _) = "*="
showOp (JSDivideAssign _) = "/="
showOp (JSModAssign _) = "%="
showOp (JSPlusAssign _) = "+="
showOp (JSMinusAssign _) = "-="
showOp (JSLshAssign _) = "<<="
showOp (JSRshAssign _) = ">>="
showOp (JSUrshAssign _) = ">>>="
showOp (JSBwAndAssign _) = "&="
showOp (JSBwXorAssign _) = "^="
showOp (JSBwOrAssign _) = "|="

stcs :: [JSTryCatch] -> String
stcs xs = "[" ++ commaJoin (map stc xs) ++ "]"

stc :: JSTryCatch -> String
stc (JSCatch _ _lb x1 _rb x3) = "JSCatch (" ++ ss x1 ++ "," ++ ssb x3 ++ ")"
stc (JSCatchIf _ _lb x1 _ ex _rb x3) = "JSCatch (" ++ ss x1 ++ ") if " ++ ss ex ++ " (" ++ ssb x3 ++ ")"

stf :: JSTryFinally -> String
stf (JSFinally _ x) = "JSFinally (" ++ ssb x ++ ")"
stf JSNoFinally = "JSFinally ()"

sst :: JSStatement -> String
sst (JSStatementBlock _ xs _ _) = "JSStatementBlock " ++ ssts xs
sst (JSBreak _ JSIdentNone s) = "JSBreak" ++ commaIf (showsemi s)
sst (JSBreak _ (JSIdentName _ n) s) = "JSBreak " ++ singleQuote n ++ commaIf (showsemi s)
sst (JSContinue _ JSIdentNone s) = "JSContinue" ++ commaIf (showsemi s)
sst (JSContinue _ (JSIdentName _ n) s) = "JSContinue " ++ singleQuote n ++ commaIf (showsemi s)
sst (JSConstant _ xs _as) = "JSConstant " ++ sses xs
sst (JSDoWhile _d x1 _w _lb x2 _rb x3) = "JSDoWhile (" ++ sst x1 ++ ") (" ++ ss x2 ++ ") (" ++ showsemi x3 ++ ")"
sst (JSFor _ _lb x1s _s1 x2s _s2 x3s _rb x4) = "JSFor " ++ sss x1s ++ " " ++ sss x2s ++ " " ++ sss x3s ++ " (" ++ sst x4 ++ ")"
sst (JSForIn _ _lb x1s _i x2 _rb x3) = "JSForIn " ++ ss x1s ++ " (" ++ ss x2 ++ ") (" ++ sst x3 ++ ")"
sst (JSForVar _ _lb _v x1s _s1 x2s _s2 x3s _rb x4) = "JSForVar " ++ sses x1s ++ " " ++ sss x2s ++ " " ++ sss x3s ++ " (" ++ sst x4 ++ ")"
sst (JSForVarIn _ _lb _v x1 _i x2 _rb x3) = "JSForVarIn (" ++ ss x1 ++ ") (" ++ ss x2 ++ ") (" ++ sst x3 ++ ")"
sst (JSFunction _ n _lb pl _rb x3 _) = "JSFunction " ++ ssid n ++ " " ++ ssjl pl ++ " (" ++ ssb x3 ++ ")"
sst (JSIf _ _lb x1 _rb x2) = "JSIf (" ++ ss x1 ++ ") (" ++ sst x2 ++ ")"
sst (JSIfElse _ _lb x1 _rb x2 _e x3) = "JSIfElse (" ++ ss x1 ++ ") (" ++ sst x2 ++ ") (" ++ sst x3 ++ ")"
sst (JSLabelled x1 _c x2) = "JSLabelled (" ++ ss x1 ++ ") (" ++ sst x2 ++ ")"
sst (JSEmptyStatement _) = "JSEmptyStatement"
sst (JSExpressionStatement l s) = ss l ++ (let x = showsemi s in if not (null x) then ',':x else "")
sst (JSAssignStatement lhs op rhs s) ="JSOpAssign (" ++ sopa op ++ "," ++ ss lhs ++ "," ++ ss rhs ++ (let x = showsemi s in if not (null x) then "),"++x else ")")
sst (JSMethodCall e a s) = "JSMemberExpression (" ++ ss e ++ "," ++ ssa a ++ (let x = showsemi s in if not (null x) then "),"++x else ")")
sst (JSReturn _ (Just me) s) = "JSReturn " ++ ss me ++ " " ++ showsemi s
sst (JSReturn _ Nothing s) = "JSReturn " ++ showsemi s
sst (JSSwitch _ _lp x _rp _lb x2 _rb _) = "JSSwitch (" ++ ss x ++ ") " ++ ssws x2
sst (JSThrow _ x _) = "JSThrow (" ++ ss x ++ ")"
sst (JSTry _ xt1 xtc xtf) = "JSTry (" ++ ssb xt1 ++ "," ++ stcs xtc ++ "," ++ stf xtf ++ ")"
sst (JSVariable _ xs _as) = "JSVariable " ++ sses xs
sst (JSWhile _ _lb x1 _rb x2) = "JSWhile (" ++ ss x1 ++ ") (" ++ sst x2 ++ ")"
sst (JSWith _ _lb x1 _rb x _) = "JSWith (" ++ ss x1 ++ ") (" ++ sst x ++ ")"

ssvi :: JSVarInitializer -> String
ssvi (JSVarInit _ n) = "[" ++ ss n ++ "]"
ssvi JSVarInitNone = ""

ssts :: [JSStatement] -> String
ssts xs = "[" ++ commaJoin (map sst xs) ++ "]"

sses :: [JSExpression] -> String
sses xs = "[" ++ commaJoin (map ss xs) ++ "]"

ssb :: JSBlock -> String
ssb (JSBlock _ xs _) = "JSBlock " ++ ssts xs

ssw :: JSSwitchParts -> String
ssw (JSCase _ x1 _c x2s) = "JSCase (" ++ ss x1 ++ ") (" ++ ssts x2s ++ ")"
ssw (JSDefault _ _c xs) = "JSDefault (" ++ ssts xs ++ ")"

ssws :: [JSSwitchParts] -> String
ssws xs = "[" ++ commaJoin (map ssw xs) ++ "]"

ssjl :: JSList JSIdent -> String
ssjl (JSParams nel) = "(" ++ commaJoin (map ssid2 $ fromNEList nel) ++ ")"
ssjl JSNoParams = "()"

ssjle :: JSList JSExpression -> String
ssjle (JSParams nel) = "(" ++ commaJoin (map ss $ fromNEList nel) ++ ")"
ssjle JSNoParams = "()"

ssa :: JSArguments -> String
ssa (JSArguments _lb xs _rb) = "JSArguments " ++ ssjle xs


commaJoin :: [String] -> String
commaJoin s = intercalate "," $ filter (not . null) s

fromNEList :: JSNonEmptyList a -> [a]
fromNEList (JSLCons l _ i) = fromNEList l ++ [i]
fromNEList (JSLOne i)      = [i]
