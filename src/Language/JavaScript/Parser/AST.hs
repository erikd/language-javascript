{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}

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
    = JSList (JSNonEmptyList a)
    | JSEmptyList
    deriving (Data, Eq, Show, Typeable)

data JSNonEmptyList a
    = JSLCons (JSNonEmptyList a) JSAnnot a -- ^head, comma, ident
    | JSLOne a
    deriving (Data, Eq, Show, Typeable)

data JSArguments
    = JSArguments JSAnnot (JSList JSExpression) JSAnnot    -- ^lb, args, rb
    deriving (Data, Eq, Show, Typeable)


-- Strip out the location info
showStripped :: JSAST -> String
showStripped (JSAstProgram xs _) = "JSAstProgram " ++ ss xs
showStripped (JSAstStatement s _) = "JSAstStatement (" ++ ss s ++ ")"
showStripped (JSAstExpression e _) = "JSAstExpression (" ++ ss e ++ ")"
showStripped (JSAstLiteral s _)  = "JSAstLiteral (" ++ ss s ++ ")"

-- -----------------------------------------------------------------------------
-- | Show the AST elements stipped of their JSAnnot data.

class ShowStripped a where
    ss :: a -> String


instance ShowStripped JSStatement where
    ss (JSStatementBlock _ xs _ _) = "JSStatementBlock " ++ ss xs
    ss (JSBreak _ JSIdentNone s) = "JSBreak" ++ commaIf (ss s)
    ss (JSBreak _ (JSIdentName _ n) s) = "JSBreak " ++ singleQuote n ++ commaIf (ss s)
    ss (JSContinue _ JSIdentNone s) = "JSContinue" ++ commaIf (ss s)
    ss (JSContinue _ (JSIdentName _ n) s) = "JSContinue " ++ singleQuote n ++ commaIf (ss s)
    ss (JSConstant _ xs _as) = "JSConstant " ++ ss xs
    ss (JSDoWhile _d x1 _w _lb x2 _rb x3) = "JSDoWhile (" ++ ss x1 ++ ") (" ++ ss x2 ++ ") (" ++ ss x3 ++ ")"
    ss (JSFor _ _lb x1s _s1 x2s _s2 x3s _rb x4) = "JSFor " ++ ss x1s ++ " " ++ ss x2s ++ " " ++ ss x3s ++ " (" ++ ss x4 ++ ")"
    ss (JSForIn _ _lb x1s _i x2 _rb x3) = "JSForIn " ++ ss x1s ++ " (" ++ ss x2 ++ ") (" ++ ss x3 ++ ")"
    ss (JSForVar _ _lb _v x1s _s1 x2s _s2 x3s _rb x4) = "JSForVar " ++ ss x1s ++ " " ++ ss x2s ++ " " ++ ss x3s ++ " (" ++ ss x4 ++ ")"
    ss (JSForVarIn _ _lb _v x1 _i x2 _rb x3) = "JSForVarIn (" ++ ss x1 ++ ") (" ++ ss x2 ++ ") (" ++ ss x3 ++ ")"
    ss (JSFunction _ n _lb pl _rb x3 _) = "JSFunction " ++ ssid n ++ " " ++ ss pl ++ " (" ++ ss x3 ++ ")"
    ss (JSIf _ _lb x1 _rb x2) = "JSIf (" ++ ss x1 ++ ") (" ++ ss x2 ++ ")"
    ss (JSIfElse _ _lb x1 _rb x2 _e x3) = "JSIfElse (" ++ ss x1 ++ ") (" ++ ss x2 ++ ") (" ++ ss x3 ++ ")"
    ss (JSLabelled x1 _c x2) = "JSLabelled (" ++ ss x1 ++ ") (" ++ ss x2 ++ ")"
    ss (JSEmptyStatement _) = "JSEmptyStatement"
    ss (JSExpressionStatement l s) = ss l ++ (let x = ss s in if not (null x) then ',':x else "")
    ss (JSAssignStatement lhs op rhs s) ="JSOpAssign (" ++ ss op ++ "," ++ ss lhs ++ "," ++ ss rhs ++ (let x = ss s in if not (null x) then "),"++x else ")")
    ss (JSMethodCall e a s) = "JSMemberExpression (" ++ ss e ++ "," ++ ss a ++ (let x = ss s in if not (null x) then "),"++x else ")")
    ss (JSReturn _ (Just me) s) = "JSReturn " ++ ss me ++ " " ++ ss s
    ss (JSReturn _ Nothing s) = "JSReturn " ++ ss s
    ss (JSSwitch _ _lp x _rp _lb x2 _rb _) = "JSSwitch (" ++ ss x ++ ") " ++ ss x2
    ss (JSThrow _ x _) = "JSThrow (" ++ ss x ++ ")"
    ss (JSTry _ xt1 xtc xtf) = "JSTry (" ++ ss xt1 ++ "," ++ ss xtc ++ "," ++ ss xtf ++ ")"
    ss (JSVariable _ xs _as) = "JSVariable " ++ ss xs
    ss (JSWhile _ _lb x1 _rb x2) = "JSWhile (" ++ ss x1 ++ ") (" ++ ss x2 ++ ")"
    ss (JSWith _ _lb x1 _rb x _) = "JSWith (" ++ ss x1 ++ ") (" ++ ss x ++ ")"

instance ShowStripped JSExpression where
    ss (JSArrayLiteral _lb xs _rb) = "JSArrayLiteral " ++ ss xs
    ss (JSAssignExpression lhs op rhs) = "JSOpAssign (" ++ ss op ++ "," ++ ss lhs ++ "," ++ ss rhs ++ ")"
    ss (JSCallExpression ex xs) = "JSCallExpression ("++ ss ex ++ "," ++ ss xs ++ ")"
    ss (JSCallExpressionDot ex _os xs) = "JSCallExpressionDot (" ++ ss ex ++ "," ++ ss xs ++ ")"
    ss (JSCallExpressionSquare ex _os xs _cs) = "JSCallExpressionSquare (" ++ ss ex ++ "," ++ ss xs ++ ")"
    ss (JSComma _) = "JSComma"
    ss (JSDecimal _ s) = "JSDecimal " ++ singleQuote s
    ss (JSCommaExpression l _ r) = "JSExpression [" ++ ss l ++ "," ++ ss r ++ "]"
    ss (JSExpressionBinary x2 op x3) = "JSExpressionBinary (" ++ ss op ++ "," ++ ss x2 ++ "," ++ ss x3 ++ ")"
    ss (JSExpressionParen _lp x _rp) = "JSExpressionParen (" ++ ss x ++ ")"
    ss (JSExpressionPostfix xs op) = "JSExpressionPostfix (" ++ ss op ++ "," ++ ss xs ++ ")"
    ss (JSExpressionTernary x1 _q x2 _c x3) = "JSExpressionTernary (" ++ ss x1 ++ "," ++ ss x2 ++ "," ++ ss x3 ++ ")"
    ss (JSFunctionExpression _ n _lb pl _rb x3) = "JSFunctionExpression " ++ ssid n ++ " " ++ ss pl ++ " (" ++ ss x3 ++ "))"
    ss (JSHexInteger _ s) = "JSHexInteger " ++ singleQuote s
    ss (JSOctal _ s) = "JSOctal " ++ singleQuote s
    ss (JSIdentifier _ s) = "JSIdentifier " ++ singleQuote s
    ss (JSLiteral _ []) = "JSLiteral ''"
    ss (JSLiteral _ s) = "JSLiteral " ++ singleQuote s
    ss (JSMemberDot x1s _d x2 ) = "JSMemberDot (" ++ ss x1s ++ "," ++ ss x2 ++ ")"
    ss (JSMemberExpression e a) = "JSMemberExpression (" ++ ss e ++ "," ++ ss a ++ ")"
    ss (JSMemberNew _a n s) = "JSMemberNew (" ++ ss n ++ "," ++ ss s ++ ")"
    ss (JSMemberSquare x1s _lb x2 _rb) = "JSMemberSquare (" ++ ss x1s ++ "," ++ ss x2 ++ ")"
    ss (JSNewExpression _n e) = "JSNewExpression " ++ ss e
    ss (JSObjectLiteral _lb xs _rb) = "JSObjectLiteral " ++ ss xs
    ss (JSPropertyNameandValue x1 _colon x2s) = "JSPropertyNameandValue (" ++ ss x1 ++ ") " ++ ss x2s
    ss (JSPropertyAccessor s x1 _lb1 x2s _rb1 x3) = "JSPropertyAccessor " ++ ss s ++ " (" ++ ss x1 ++ ") " ++ ss x2s ++ " (" ++ ss x3 ++ ")"
    ss (JSRegEx _ s) = "JSRegEx " ++ singleQuote s
    ss (JSStringLiteralS _ s) = "JSStringLiteralS " ++ singleQuote s
    ss (JSStringLiteralD _ s) = "JSStringLiteralD " ++ singleQuote s
    ss (JSUnaryExpression op x) = "JSUnaryExpression (" ++ ss op ++ "," ++ ss x ++ ")"
    ss (JSVarInitExpression x1 x2) = "JSVarInitExpression (" ++ ss x1 ++ ") " ++ ss x2

instance ShowStripped JSTryCatch where
    ss (JSCatch _ _lb x1 _rb x3) = "JSCatch (" ++ ss x1 ++ "," ++ ss x3 ++ ")"
    ss (JSCatchIf _ _lb x1 _ ex _rb x3) = "JSCatch (" ++ ss x1 ++ ") if " ++ ss ex ++ " (" ++ ss x3 ++ ")"

instance ShowStripped JSTryFinally where
    ss (JSFinally _ x) = "JSFinally (" ++ ss x ++ ")"
    ss JSNoFinally = "JSFinally ()"

instance ShowStripped JSIdent where
    ss (JSIdentName _ s) = "JSIdentifier " ++ singleQuote s
    ss JSIdentNone = "JSIdentNone"

instance ShowStripped JSAccessor where
    ss (JSAccessorGet _) = "JSAccessorGet"
    ss (JSAccessorSet _) = "JSAccessorSet"

instance ShowStripped JSBlock where
    ss (JSBlock _ xs _) = "JSBlock " ++ ss xs

instance ShowStripped JSSwitchParts where
    ss (JSCase _ x1 _c x2s) = "JSCase (" ++ ss x1 ++ ") (" ++ ss x2s ++ ")"
    ss (JSDefault _ _c xs) = "JSDefault (" ++ ss xs ++ ")"

instance ShowStripped JSBinOp where
    ss (JSBinOpAnd _) = "'&&'"
    ss (JSBinOpBitAnd _) = "'&'"
    ss (JSBinOpBitOr _) = "'|'"
    ss (JSBinOpBitXor _) = "'^'"
    ss (JSBinOpDivide _) = "'/'"
    ss (JSBinOpEq _) = "'=='"
    ss (JSBinOpGe _) = "'>='"
    ss (JSBinOpGt _) = "'>'"
    ss (JSBinOpIn _) = "'in'"
    ss (JSBinOpInstanceOf _) = "'instanceof'"
    ss (JSBinOpLe _) = "'<='"
    ss (JSBinOpLsh _) = "'<<'"
    ss (JSBinOpLt _) = "'<'"
    ss (JSBinOpMinus _) = "'-'"
    ss (JSBinOpMod _) = "'%'"
    ss (JSBinOpNeq _) = "'!='"
    ss (JSBinOpOr _) = "'||'"
    ss (JSBinOpPlus _) = "'+'"
    ss (JSBinOpRsh _) = "'>>'"
    ss (JSBinOpStrictEq _) = "'==='"
    ss (JSBinOpStrictNeq _) = "'!=='"
    ss (JSBinOpTimes _) = "'*'"
    ss (JSBinOpUrsh _) = "'>>>'"

instance ShowStripped JSUnaryOp where
    ss (JSUnaryOpDecr _) = "'--'"
    ss (JSUnaryOpDelete _) = "'delete'"
    ss (JSUnaryOpIncr _) = "'++'"
    ss (JSUnaryOpMinus _) = "'-'"
    ss (JSUnaryOpNot _) = "'!'"
    ss (JSUnaryOpPlus _) = "'+'"
    ss (JSUnaryOpTilde _) = "'~'"
    ss (JSUnaryOpTypeof _) = "'typeof'"
    ss (JSUnaryOpVoid _) = "'void'"

instance ShowStripped JSAssignOp where
    ss (JSAssign _) = "'='"
    ss (JSTimesAssign _) = "'*='"
    ss (JSDivideAssign _) = "'/='"
    ss (JSModAssign _) = "'%='"
    ss (JSPlusAssign _) = "'+='"
    ss (JSMinusAssign _) = "'-='"
    ss (JSLshAssign _) = "'<<='"
    ss (JSRshAssign _) = "'>>='"
    ss (JSUrshAssign _) = "'>>>='"
    ss (JSBwAndAssign _) = "'&='"
    ss (JSBwXorAssign _) = "'^='"
    ss (JSBwOrAssign _) = "'|='"

instance ShowStripped JSVarInitializer where
    ss (JSVarInit _ n) = "[" ++ ss n ++ "]"
    ss JSVarInitNone = ""

instance ShowStripped JSArguments where
    ss (JSArguments _lb xs _rb) = "JSArguments " ++ ss xs

instance ShowStripped JSSemi where
    ss (JSSemi _) = "JSSemicolon"
    ss JSSemiAuto = ""


instance ShowStripped a => ShowStripped (JSList a) where
    ss (JSList nel) = "(" ++ commaJoin (map ss $ fromNEList nel) ++ ")"
    ss JSEmptyList = "()"

instance ShowStripped a => ShowStripped [a] where
    ss xs = "[" ++ commaJoin (map ss xs) ++ "]"

-- -----------------------------------------------------------------------------
-- Helpers.

commaJoin :: [String] -> String
commaJoin s = intercalate "," $ filter (not . null) s

fromNEList :: JSNonEmptyList a -> [a]
fromNEList (JSLCons l _ i) = fromNEList l ++ [i]
fromNEList (JSLOne i)      = [i]

singleQuote :: String -> String
singleQuote s = '\'' : (s ++ "'")

ssid :: JSIdent -> String
ssid (JSIdentName _ s) = singleQuote s
ssid JSIdentNone = "''"

commaIf :: String -> String
commaIf "" = ""
commaIf xs = ',' : xs
