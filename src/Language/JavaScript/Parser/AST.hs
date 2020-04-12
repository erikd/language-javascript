{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.JavaScript.Parser.AST
  ( JSExpression (..),
    JSAnnot (..),
    JSBinOp (..),
    JSUnaryOp (..),
    JSSemi (..),
    JSAssignOp (..),
    JSTryCatch (..),
    JSTryFinally (..),
    JSStatement (..),
    JSBlock (..),
    JSSwitchParts (..),
    JSAST (..),
    JSObjectProperty (..),
    JSPropertyName (..),
    JSObjectPropertyList,
    JSAccessor (..),
    JSMethodDefinition (..),
    JSIdent (..),
    JSVarInitializer (..),
    JSArrayElement (..),
    JSCommaList (..),
    JSCommaTrailingList (..),
    JSArrowParameterList (..),
    JSTemplatePart (..),
    JSClassHeritage (..),
    JSClassElement (..),
    -- Modules
    JSModuleItem (..),
    JSImportDeclaration (..),
    JSImportClause (..),
    JSFromClause (..),
    JSImportNameSpace (..),
    JSImportsNamed (..),
    JSImportSpecifier (..),
    JSExportDeclaration (..),
    JSExportClause (..),
    JSExportSpecifier (..),
    binOpEq,
    showStripped,
  )
where

import Data.Data
import Data.List
import Language.JavaScript.Parser.SrcLocation (TokenPosn (..))
import Language.JavaScript.Parser.Token

-- ---------------------------------------------------------------------

data JSAnnot
  = -- | Annotation: position and comment/whitespace information
    JSAnnot !TokenPosn ![CommentAnnotation]
  | -- | A single space character
    JSAnnotSpace
  | -- | No annotation
    JSNoAnnot
  deriving (Data, Eq, Show, Typeable)

data JSAST
  = -- | source elements, trailing whitespace
    JSAstProgram ![JSStatement] !JSAnnot
  | JSAstModule ![JSModuleItem] !JSAnnot
  | JSAstStatement !JSStatement !JSAnnot
  | JSAstExpression !JSExpression !JSAnnot
  | JSAstLiteral !JSExpression !JSAnnot
  deriving (Data, Eq, Show, Typeable)

-- Shift AST
-- https://github.com/shapesecurity/shift-spec/blob/83498b92c436180cc0e2115b225a68c08f43c53e/spec.idl#L229-L234
data JSModuleItem
  = -- | import,decl
    JSModuleImportDeclaration !JSAnnot !JSImportDeclaration
  | -- | export,decl
    JSModuleExportDeclaration !JSAnnot !JSExportDeclaration
  | JSModuleStatementListItem !JSStatement
  deriving (Data, Eq, Show, Typeable)

data JSImportDeclaration
  = -- | imports, module, semi
    JSImportDeclaration !JSImportClause !JSFromClause !JSSemi
  | -- | module, module, semi
    JSImportDeclarationBare !JSAnnot !String !JSSemi
  deriving (Data, Eq, Show, Typeable)

data JSImportClause
  = -- | default
    JSImportClauseDefault !JSIdent
  | -- | namespace
    JSImportClauseNameSpace !JSImportNameSpace
  | -- | named imports
    JSImportClauseNamed !JSImportsNamed
  | -- | default, comma, namespace
    JSImportClauseDefaultNameSpace !JSIdent !JSAnnot !JSImportNameSpace
  | -- | default, comma, named imports
    JSImportClauseDefaultNamed !JSIdent !JSAnnot !JSImportsNamed
  deriving (Data, Eq, Show, Typeable)

data JSFromClause
  = -- | from, string literal, string literal contents
    JSFromClause !JSAnnot !JSAnnot !String
  deriving (Data, Eq, Show, Typeable)

-- | Import namespace, e.g. '* as whatever'
data JSImportNameSpace
  = -- | *, as, ident
    JSImportNameSpace !JSBinOp !JSAnnot !JSIdent
  deriving (Data, Eq, Show, Typeable)

-- | Named imports, e.g. '{ foo, bar, baz as quux }'
data JSImportsNamed
  = -- | lb, specifiers, rb
    JSImportsNamed !JSAnnot !(JSCommaList JSImportSpecifier) !JSAnnot
  deriving (Data, Eq, Show, Typeable)

-- |
-- Note that this data type is separate from ExportSpecifier because the
-- grammar is slightly different (e.g. in handling of reserved words).
data JSImportSpecifier
  = -- | ident
    JSImportSpecifier !JSIdent
  | -- | ident, as, ident
    JSImportSpecifierAs !JSIdent !JSAnnot !JSIdent
  deriving (Data, Eq, Show, Typeable)

data JSExportDeclaration
  = -- = JSExportAllFrom

    -- | exports, module, semi
    JSExportFrom JSExportClause JSFromClause !JSSemi
  | -- | exports, autosemi
    JSExportLocals JSExportClause !JSSemi
  | -- | body, autosemi
    --  | JSExportDefault
    JSExport !JSStatement !JSSemi
  deriving (Data, Eq, Show, Typeable)

data JSExportClause
  = -- | lb, specifiers, rb
    JSExportClause !JSAnnot !(JSCommaList JSExportSpecifier) !JSAnnot
  deriving (Data, Eq, Show, Typeable)

data JSExportSpecifier
  = -- | ident
    JSExportSpecifier !JSIdent
  | -- | ident1, as, ident2
    JSExportSpecifierAs !JSIdent !JSAnnot !JSIdent
  deriving (Data, Eq, Show, Typeable)

data JSStatement
  = -- | lbrace, stmts, rbrace, autosemi
    JSStatementBlock !JSAnnot ![JSStatement] !JSAnnot !JSSemi
  | -- | break,optional identifier, autosemi
    JSBreak !JSAnnot !JSIdent !JSSemi
  | -- | const, decl, autosemi
    JSConstant !JSAnnot !(JSCommaList JSExpression) !JSSemi
  | -- | fn,name, lb,parameter list,rb,block,autosemi
    JSFunction !JSAnnot !JSIdent !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock !JSSemi
  | -- | if,(,expr,),stmt
    JSIf !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSStatement
  | -- | if,(,expr,),stmt,else,rest
    JSIfElse !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSStatement !JSAnnot !JSStatement
  | -- | identifier,colon,stmt
    JSLabelled !JSIdent !JSAnnot !JSStatement
  | JSEmptyStatement !JSAnnot
  | JSExpressionStatement !JSExpression !JSSemi
  | JSMethodCall !JSExpression !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSSemi
  | -- | optional expression,autosemi
    JSReturn !JSAnnot !(Maybe JSExpression) !JSSemi
  | -- | switch,lb,expr,rb,caseblock,autosemi
    JSSwitch !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSAnnot ![JSSwitchParts] !JSAnnot !JSSemi
  | -- | throw val autosemi
    JSThrow !JSAnnot !JSExpression !JSSemi
  deriving (Data, Eq, Show, Typeable)

data JSExpression
  = -- | Terminals
    JSIdentifier !JSAnnot !String
  | JSDecimal !JSAnnot !String
  | JSLiteral !JSAnnot !String
  | JSHexInteger !JSAnnot !String
  | JSOctal !JSAnnot !String
  | JSStringLiteral !JSAnnot !String
  | JSRegEx !JSAnnot !String
  | -- | lb, contents, rb
    JSArrayLiteral !JSAnnot ![JSArrayElement] !JSAnnot
  | -- | lhs, assignop, rhs
    JSCallExpression !JSExpression !JSAnnot !(JSCommaList JSExpression) !JSAnnot
  | -- | expr, dot, expr
    JSCallExpressionDot !JSExpression !JSAnnot !JSExpression
  | -- | expr, [, expr, ]
    JSCallExpressionSquare !JSExpression !JSAnnot !JSExpression !JSAnnot
  | -- | class, optional identifier, optional extends clause, lb, body, rb
    JSClassExpression !JSAnnot !JSIdent !JSClassHeritage !JSAnnot ![JSClassElement] !JSAnnot
  | -- | expression components
    JSCommaExpression !JSExpression !JSAnnot !JSExpression
  | -- | lhs, op, rhs
    JSExpressionBinary !JSExpression !JSBinOp !JSExpression
  | -- | lb,expression,rb
    JSExpressionParen !JSAnnot !JSExpression !JSAnnot
  | -- | expression, operator
    JSExpressionPostfix !JSExpression !JSUnaryOp
  | -- | cond, ?, trueval, :, falseval
    JSExpressionTernary !JSExpression !JSAnnot !JSExpression !JSAnnot !JSExpression
  | -- | parameter list,arrow,block`
    JSArrowExpression !JSArrowParameterList !JSAnnot !JSStatement
  | -- | fn,name,lb, parameter list,rb,block`
    JSFunctionExpression !JSAnnot !JSIdent !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock
  | -- | fn,*,name,lb, parameter list,rb,block`
    JSMemberDot !JSExpression !JSAnnot !JSExpression
  | JSMemberExpression !JSExpression !JSAnnot !(JSCommaList JSExpression) !JSAnnot -- expr, lb, args, rb
  | -- | new, name, lb, args, rb
    JSMemberNew !JSAnnot !JSExpression !JSAnnot !(JSCommaList JSExpression) !JSAnnot
  | -- | firstpart, lb, expr, rb
    JSMemberSquare !JSExpression !JSAnnot !JSExpression !JSAnnot
  | -- | lbrace contents rbrace
    JSObjectLiteral !JSAnnot !JSObjectPropertyList !JSAnnot
  | JSSpreadExpression !JSAnnot !JSExpression
  | -- | optional tag, lquot, head, parts
    JSTemplateLiteral !(Maybe JSExpression) !JSAnnot !String ![JSTemplatePart]
  | JSUnaryExpression !JSUnaryOp !JSExpression
  | -- | identifier, initializer
    JSVarInitExpression !JSExpression !JSVarInitializer
  deriving (Data, Eq, Show, Typeable)

data JSArrowParameterList
  = JSUnparenthesizedArrowParameter !JSIdent
  | JSParenthesizedArrowParameterList !JSAnnot !(JSCommaList JSExpression) !JSAnnot
  deriving (Data, Eq, Show, Typeable)

data JSBinOp
  = JSBinOpAnd !JSAnnot
  | JSBinOpBitAnd !JSAnnot
  | JSBinOpBitOr !JSAnnot
  | JSBinOpBitXor !JSAnnot
  | JSBinOpDivide !JSAnnot
  | JSBinOpEq !JSAnnot
  | JSBinOpGe !JSAnnot
  | JSBinOpGt !JSAnnot
  | JSBinOpIn !JSAnnot
  | JSBinOpInstanceOf !JSAnnot
  | JSBinOpLe !JSAnnot
  | JSBinOpLsh !JSAnnot
  | JSBinOpLt !JSAnnot
  | JSBinOpMinus !JSAnnot
  | JSBinOpMod !JSAnnot
  | JSBinOpNeq !JSAnnot
  | JSBinOpOf !JSAnnot
  | JSBinOpOr !JSAnnot
  | JSBinOpPlus !JSAnnot
  | JSBinOpRsh !JSAnnot
  | JSBinOpStrictEq !JSAnnot
  | JSBinOpStrictNeq !JSAnnot
  | JSBinOpTimes !JSAnnot
  | JSBinOpUrsh !JSAnnot
  deriving (Data, Eq, Show, Typeable)

data JSUnaryOp
  = JSUnaryOpDecr !JSAnnot
  | JSUnaryOpDelete !JSAnnot
  | JSUnaryOpIncr !JSAnnot
  | JSUnaryOpMinus !JSAnnot
  | JSUnaryOpNot !JSAnnot
  | JSUnaryOpPlus !JSAnnot
  | JSUnaryOpTilde !JSAnnot
  | JSUnaryOpTypeof !JSAnnot
  | JSUnaryOpVoid !JSAnnot
  deriving (Data, Eq, Show, Typeable)

data JSSemi
  = JSSemi !JSAnnot
  | JSSemiAuto
  deriving (Data, Eq, Show, Typeable)

data JSAssignOp
  = JSAssign !JSAnnot
  | JSTimesAssign !JSAnnot
  | JSDivideAssign !JSAnnot
  | JSModAssign !JSAnnot
  | JSPlusAssign !JSAnnot
  | JSMinusAssign !JSAnnot
  | JSLshAssign !JSAnnot
  | JSRshAssign !JSAnnot
  | JSUrshAssign !JSAnnot
  | JSBwAndAssign !JSAnnot
  | JSBwXorAssign !JSAnnot
  | JSBwOrAssign !JSAnnot
  deriving (Data, Eq, Show, Typeable)

data JSTryCatch
  = -- | catch,lb,ident,rb,block
    JSCatch !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSBlock
  | -- | catch,lb,ident,if,expr,rb,block
    JSCatchIf !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSExpression !JSAnnot !JSBlock
  deriving (Data, Eq, Show, Typeable)

data JSTryFinally
  = -- | finally,block
    JSFinally !JSAnnot !JSBlock
  | JSNoFinally
  deriving (Data, Eq, Show, Typeable)

data JSBlock
  = -- | lbrace, stmts, rbrace
    JSBlock !JSAnnot ![JSStatement] !JSAnnot
  deriving (Data, Eq, Show, Typeable)

data JSSwitchParts
  = -- | expr,colon,stmtlist
    JSCase !JSAnnot !JSExpression !JSAnnot ![JSStatement]
  | -- | colon,stmtlist
    JSDefault !JSAnnot !JSAnnot ![JSStatement]
  deriving (Data, Eq, Show, Typeable)

data JSVarInitializer
  = -- | assignop, initializer
    JSVarInit !JSAnnot !JSExpression
  | JSVarInitNone
  deriving (Data, Eq, Show, Typeable)

data JSObjectProperty
  = -- | name, colon, value
    JSPropertyNameandValue !JSPropertyName !JSAnnot ![JSExpression]
  | JSPropertyIdentRef !JSAnnot !String
  | JSObjectMethod !JSMethodDefinition
  deriving (Data, Eq, Show, Typeable)

data JSMethodDefinition
  = JSMethodDefinition !JSPropertyName !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock -- name, lb, params, rb, block
  | -- | *, name, lb, params, rb, block
    JSGeneratorMethodDefinition !JSAnnot !JSPropertyName !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock
  | -- | get/set, name, lb, params, rb, block
    JSPropertyAccessor !JSAccessor !JSPropertyName !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock
  deriving (Data, Eq, Show, Typeable)

data JSPropertyName
  = JSPropertyIdent !JSAnnot !String
  | JSPropertyString !JSAnnot !String
  | JSPropertyNumber !JSAnnot !String
  | -- | lb, expr, rb
    JSPropertyComputed !JSAnnot !JSExpression !JSAnnot
  deriving (Data, Eq, Show, Typeable)

type JSObjectPropertyList = JSCommaTrailingList JSObjectProperty

-- | Accessors for JSObjectProperty is either 'get' or 'set'.
data JSAccessor
  = JSAccessorGet !JSAnnot
  | JSAccessorSet !JSAnnot
  deriving (Data, Eq, Show, Typeable)

data JSIdent
  = JSIdentName !JSAnnot !String
  | JSIdentNone
  deriving (Data, Eq, Show, Typeable)

data JSArrayElement
  = JSArrayElement !JSExpression
  | JSArrayComma !JSAnnot
  deriving (Data, Eq, Show, Typeable)

data JSCommaList a
  = -- | head, comma, a
    JSLCons !(JSCommaList a) !JSAnnot !a
  | -- | single element (no comma)
    JSLOne !a
  | JSLNil
  deriving (Data, Eq, Show, Typeable)

data JSCommaTrailingList a
  = -- | list, trailing comma
    JSCTLComma !(JSCommaList a) !JSAnnot
  | -- | list
    JSCTLNone !(JSCommaList a)
  deriving (Data, Eq, Show, Typeable)

data JSTemplatePart
  = -- | expr, rb, suffix
    JSTemplatePart !JSExpression !JSAnnot !String
  deriving (Data, Eq, Show, Typeable)

data JSClassHeritage
  = JSExtends !JSAnnot !JSExpression
  | JSExtendsNone
  deriving (Data, Eq, Show, Typeable)

data JSClassElement
  = JSClassInstanceMethod !JSMethodDefinition
  | JSClassStaticMethod !JSAnnot !JSMethodDefinition
  | JSClassSemi !JSAnnot
  deriving (Data, Eq, Show, Typeable)

-- -----------------------------------------------------------------------------

-- | Show the AST elements stripped of their JSAnnot data.
showStripped ::
  -- Strip out the location info
  JSAST -> String
showStripped (JSAstProgram xs _) = "JSAstProgram " ++ ss xs
showStripped (JSAstModule xs _) = "JSAstModule " ++ ss xs
showStripped (JSAstStatement s _) = "JSAstStatement (" ++ ss s ++ ")"
showStripped (JSAstExpression e _) = "JSAstExpression (" ++ ss e ++ ")"
showStripped (JSAstLiteral s _) = "JSAstLiteral (" ++ ss s ++ ")"

class ShowStripped a where
  ss :: a -> String

instance ShowStripped JSStatement where
  ss (JSStatementBlock _ xs _ _) = "JSStatementBlock " ++ ss xs
  ss (JSBreak _ JSIdentNone s) = "JSBreak" ++ commaIf (ss s)
  ss (JSBreak _ (JSIdentName _ n) s) = "JSBreak " ++ singleQuote n ++ commaIf (ss s)
  ss (JSConstant _ xs _as) = "JSConstant " ++ ss xs
  ss (JSFunction _ n _lb pl _rb x3 _) = "JSFunction " ++ ssid n ++ " " ++ ss pl ++ " (" ++ ss x3 ++ ")"
  ss (JSIf _ _lb x1 _rb x2) = "JSIf (" ++ ss x1 ++ ") (" ++ ss x2 ++ ")"
  ss (JSIfElse _ _lb x1 _rb x2 _e x3) = "JSIfElse (" ++ ss x1 ++ ") (" ++ ss x2 ++ ") (" ++ ss x3 ++ ")"
  ss (JSLabelled x1 _c x2) = "JSLabelled (" ++ ss x1 ++ ") (" ++ ss x2 ++ ")"
  ss (JSEmptyStatement _) = "JSEmptyStatement"
  ss (JSExpressionStatement l s) = ss l ++ (let x = ss s in if not (null x) then ',' : x else "")
  ss (JSMethodCall e _ a _ s) = "JSMethodCall (" ++ ss e ++ ",JSArguments " ++ ss a ++ (let x = ss s in if not (null x) then ")," ++ x else ")")
  ss (JSReturn _ (Just me) s) = "JSReturn " ++ ss me ++ " " ++ ss s
  ss (JSReturn _ Nothing s) = "JSReturn " ++ ss s
  ss (JSSwitch _ _lp x _rp _lb x2 _rb _) = "JSSwitch (" ++ ss x ++ ") " ++ ss x2
  ss (JSThrow _ x _) = "JSThrow (" ++ ss x ++ ")"

instance ShowStripped JSExpression where
  ss (JSArrayLiteral _lb xs _rb) = "JSArrayLiteral " ++ ss xs
  ss (JSCallExpression ex _ xs _) = "JSCallExpression (" ++ ss ex ++ ",JSArguments " ++ ss xs ++ ")"
  ss (JSCallExpressionDot ex _os xs) = "JSCallExpressionDot (" ++ ss ex ++ "," ++ ss xs ++ ")"
  ss (JSCallExpressionSquare ex _os xs _cs) = "JSCallExpressionSquare (" ++ ss ex ++ "," ++ ss xs ++ ")"
  ss (JSClassExpression _ n h _lb xs _rb) = "JSClassExpression " ++ ssid n ++ " (" ++ ss h ++ ") " ++ ss xs
  ss (JSDecimal _ s) = "JSDecimal " ++ singleQuote s
  ss (JSCommaExpression l _ r) = "JSExpression [" ++ ss l ++ "," ++ ss r ++ "]"
  ss (JSExpressionBinary x2 op x3) = "JSExpressionBinary (" ++ ss op ++ "," ++ ss x2 ++ "," ++ ss x3 ++ ")"
  ss (JSExpressionParen _lp x _rp) = "JSExpressionParen (" ++ ss x ++ ")"
  ss (JSExpressionPostfix xs op) = "JSExpressionPostfix (" ++ ss op ++ "," ++ ss xs ++ ")"
  ss (JSExpressionTernary x1 _q x2 _c x3) = "JSExpressionTernary (" ++ ss x1 ++ "," ++ ss x2 ++ "," ++ ss x3 ++ ")"
  ss (JSArrowExpression ps _ e) = "JSArrowExpression (" ++ ss ps ++ ") => " ++ ss e
  ss (JSFunctionExpression _ n _lb pl _rb x3) = "JSFunctionExpression " ++ ssid n ++ " " ++ ss pl ++ " (" ++ ss x3 ++ ")"
  ss (JSHexInteger _ s) = "JSHexInteger " ++ singleQuote s
  ss (JSOctal _ s) = "JSOctal " ++ singleQuote s
  ss (JSIdentifier _ s) = "JSIdentifier " ++ singleQuote s
  ss (JSLiteral _ []) = "JSLiteral ''"
  ss (JSLiteral _ s) = "JSLiteral " ++ singleQuote s
  ss (JSMemberDot x1s _d x2) = "JSMemberDot (" ++ ss x1s ++ "," ++ ss x2 ++ ")"
  ss (JSMemberExpression e _ a _) = "JSMemberExpression (" ++ ss e ++ ",JSArguments " ++ ss a ++ ")"
  ss (JSMemberNew _a n _ s _) = "JSMemberNew (" ++ ss n ++ ",JSArguments " ++ ss s ++ ")"
  ss (JSMemberSquare x1s _lb x2 _rb) = "JSMemberSquare (" ++ ss x1s ++ "," ++ ss x2 ++ ")"
  ss (JSObjectLiteral _lb xs _rb) = "JSObjectLiteral " ++ ss xs
  ss (JSRegEx _ s) = "JSRegEx " ++ singleQuote s
  ss (JSStringLiteral _ s) = "JSStringLiteral " ++ s
  ss (JSUnaryExpression op x) = "JSUnaryExpression (" ++ ss op ++ "," ++ ss x ++ ")"
  ss (JSVarInitExpression x1 x2) = "JSVarInitExpression (" ++ ss x1 ++ ") " ++ ss x2
  ss (JSSpreadExpression _ x1) = "JSSpreadExpression (" ++ ss x1 ++ ")"
  ss (JSTemplateLiteral Nothing _ s ps) = "JSTemplateLiteral (()," ++ singleQuote s ++ "," ++ ss ps ++ ")"
  ss (JSTemplateLiteral (Just t) _ s ps) = "JSTemplateLiteral ((" ++ ss t ++ ")," ++ singleQuote s ++ "," ++ ss ps ++ ")"

instance ShowStripped JSArrowParameterList where
  ss (JSUnparenthesizedArrowParameter x) = ss x
  ss (JSParenthesizedArrowParameterList _ xs _) = ss xs

instance ShowStripped JSModuleItem where
  ss (JSModuleExportDeclaration _ x1) = "JSModuleExportDeclaration (" ++ ss x1 ++ ")"
  ss (JSModuleImportDeclaration _ x1) = "JSModuleImportDeclaration (" ++ ss x1 ++ ")"
  ss (JSModuleStatementListItem x1) = "JSModuleStatementListItem (" ++ ss x1 ++ ")"

instance ShowStripped JSImportDeclaration where
  ss (JSImportDeclaration imp from _) = "JSImportDeclaration (" ++ ss imp ++ "," ++ ss from ++ ")"
  ss (JSImportDeclarationBare _ m _) = "JSImportDeclarationBare (" ++ singleQuote m ++ ")"

instance ShowStripped JSImportClause where
  ss (JSImportClauseDefault x) = "JSImportClauseDefault (" ++ ss x ++ ")"
  ss (JSImportClauseNameSpace x) = "JSImportClauseNameSpace (" ++ ss x ++ ")"
  ss (JSImportClauseNamed x) = "JSImportClauseNameSpace (" ++ ss x ++ ")"
  ss (JSImportClauseDefaultNameSpace x1 _ x2) = "JSImportClauseDefaultNameSpace (" ++ ss x1 ++ "," ++ ss x2 ++ ")"
  ss (JSImportClauseDefaultNamed x1 _ x2) = "JSImportClauseDefaultNamed (" ++ ss x1 ++ "," ++ ss x2 ++ ")"

instance ShowStripped JSFromClause where
  ss (JSFromClause _ _ m) = "JSFromClause " ++ singleQuote m

instance ShowStripped JSImportNameSpace where
  ss (JSImportNameSpace _ _ x) = "JSImportNameSpace (" ++ ss x ++ ")"

instance ShowStripped JSImportsNamed where
  ss (JSImportsNamed _ xs _) = "JSImportsNamed (" ++ ss xs ++ ")"

instance ShowStripped JSImportSpecifier where
  ss (JSImportSpecifier x1) = "JSImportSpecifier (" ++ ss x1 ++ ")"
  ss (JSImportSpecifierAs x1 _ x2) = "JSImportSpecifierAs (" ++ ss x1 ++ "," ++ ss x2 ++ ")"

instance ShowStripped JSExportDeclaration where
  ss (JSExportFrom xs from _) = "JSExportFrom (" ++ ss xs ++ "," ++ ss from ++ ")"
  ss (JSExportLocals xs _) = "JSExportLocals (" ++ ss xs ++ ")"
  ss (JSExport x1 _) = "JSExport (" ++ ss x1 ++ ")"

instance ShowStripped JSExportClause where
  ss (JSExportClause _ xs _) = "JSExportClause (" ++ ss xs ++ ")"

instance ShowStripped JSExportSpecifier where
  ss (JSExportSpecifier x1) = "JSExportSpecifier (" ++ ss x1 ++ ")"
  ss (JSExportSpecifierAs x1 _ x2) = "JSExportSpecifierAs (" ++ ss x1 ++ "," ++ ss x2 ++ ")"

instance ShowStripped JSTryCatch where
  ss (JSCatch _ _lb x1 _rb x3) = "JSCatch (" ++ ss x1 ++ "," ++ ss x3 ++ ")"
  ss (JSCatchIf _ _lb x1 _ ex _rb x3) = "JSCatch (" ++ ss x1 ++ ") if " ++ ss ex ++ " (" ++ ss x3 ++ ")"

instance ShowStripped JSTryFinally where
  ss (JSFinally _ x) = "JSFinally (" ++ ss x ++ ")"
  ss JSNoFinally = "JSFinally ()"

instance ShowStripped JSIdent where
  ss (JSIdentName _ s) = "JSIdentifier " ++ singleQuote s
  ss JSIdentNone = "JSIdentNone"

instance ShowStripped JSObjectProperty where
  ss (JSPropertyNameandValue x1 _colon x2s) = "JSPropertyNameandValue (" ++ ss x1 ++ ") " ++ ss x2s
  ss (JSPropertyIdentRef _ s) = "JSPropertyIdentRef " ++ singleQuote s
  ss (JSObjectMethod m) = ss m

instance ShowStripped JSMethodDefinition where
  ss (JSMethodDefinition x1 _lb1 x2s _rb1 x3) = "JSMethodDefinition (" ++ ss x1 ++ ") " ++ ss x2s ++ " (" ++ ss x3 ++ ")"
  ss (JSPropertyAccessor s x1 _lb1 x2s _rb1 x3) = "JSPropertyAccessor " ++ ss s ++ " (" ++ ss x1 ++ ") " ++ ss x2s ++ " (" ++ ss x3 ++ ")"
  ss (JSGeneratorMethodDefinition _ x1 _lb1 x2s _rb1 x3) = "JSGeneratorMethodDefinition (" ++ ss x1 ++ ") " ++ ss x2s ++ " (" ++ ss x3 ++ ")"

instance ShowStripped JSPropertyName where
  ss (JSPropertyIdent _ s) = "JSIdentifier " ++ singleQuote s
  ss (JSPropertyString _ s) = "JSIdentifier " ++ singleQuote s
  ss (JSPropertyNumber _ s) = "JSIdentifier " ++ singleQuote s
  ss (JSPropertyComputed _ x _) = "JSPropertyComputed (" ++ ss x ++ ")"

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
  ss (JSBinOpOf _) = "'of'"
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

instance ShowStripped JSSemi where
  ss (JSSemi _) = "JSSemicolon"
  ss JSSemiAuto = ""

instance ShowStripped JSArrayElement where
  ss (JSArrayElement e) = ss e
  ss (JSArrayComma _) = "JSComma"

instance ShowStripped JSTemplatePart where
  ss (JSTemplatePart e _ s) = "(" ++ ss e ++ "," ++ singleQuote s ++ ")"

instance ShowStripped JSClassHeritage where
  ss JSExtendsNone = ""
  ss (JSExtends _ x) = ss x

instance ShowStripped JSClassElement where
  ss (JSClassInstanceMethod m) = ss m
  ss (JSClassStaticMethod _ m) = "JSClassStaticMethod (" ++ ss m ++ ")"
  ss (JSClassSemi _) = "JSClassSemi"

instance ShowStripped a => ShowStripped (JSCommaList a) where
  ss xs = "(" ++ commaJoin (map ss $ fromCommaList xs) ++ ")"

instance ShowStripped a => ShowStripped (JSCommaTrailingList a) where
  ss (JSCTLComma xs _) = "[" ++ commaJoin (map ss $ fromCommaList xs) ++ ",JSComma]"
  ss (JSCTLNone xs) = "[" ++ commaJoin (map ss $ fromCommaList xs) ++ "]"

instance ShowStripped a => ShowStripped [a] where
  ss xs = "[" ++ commaJoin (map ss xs) ++ "]"

-- -----------------------------------------------------------------------------
-- Helpers.

commaJoin :: [String] -> String
commaJoin s = intercalate "," $ filter (not . null) s

fromCommaList :: JSCommaList a -> [a]
fromCommaList (JSLCons l _ i) = fromCommaList l ++ [i]
fromCommaList (JSLOne i) = [i]
fromCommaList JSLNil = []

singleQuote :: String -> String
singleQuote s = '\'' : (s ++ "'")

ssid :: JSIdent -> String
ssid (JSIdentName _ s) = singleQuote s
ssid JSIdentNone = "''"

commaIf :: String -> String
commaIf "" = ""
commaIf xs = ',' : xs

deAnnot :: JSBinOp -> JSBinOp
deAnnot (JSBinOpAnd _) = JSBinOpAnd JSNoAnnot
deAnnot (JSBinOpBitAnd _) = JSBinOpBitAnd JSNoAnnot
deAnnot (JSBinOpBitOr _) = JSBinOpBitOr JSNoAnnot
deAnnot (JSBinOpBitXor _) = JSBinOpBitXor JSNoAnnot
deAnnot (JSBinOpDivide _) = JSBinOpDivide JSNoAnnot
deAnnot (JSBinOpEq _) = JSBinOpEq JSNoAnnot
deAnnot (JSBinOpGe _) = JSBinOpGe JSNoAnnot
deAnnot (JSBinOpGt _) = JSBinOpGt JSNoAnnot
deAnnot (JSBinOpIn _) = JSBinOpIn JSNoAnnot
deAnnot (JSBinOpInstanceOf _) = JSBinOpInstanceOf JSNoAnnot
deAnnot (JSBinOpLe _) = JSBinOpLe JSNoAnnot
deAnnot (JSBinOpLsh _) = JSBinOpLsh JSNoAnnot
deAnnot (JSBinOpLt _) = JSBinOpLt JSNoAnnot
deAnnot (JSBinOpMinus _) = JSBinOpMinus JSNoAnnot
deAnnot (JSBinOpMod _) = JSBinOpMod JSNoAnnot
deAnnot (JSBinOpNeq _) = JSBinOpNeq JSNoAnnot
deAnnot (JSBinOpOf _) = JSBinOpOf JSNoAnnot
deAnnot (JSBinOpOr _) = JSBinOpOr JSNoAnnot
deAnnot (JSBinOpPlus _) = JSBinOpPlus JSNoAnnot
deAnnot (JSBinOpRsh _) = JSBinOpRsh JSNoAnnot
deAnnot (JSBinOpStrictEq _) = JSBinOpStrictEq JSNoAnnot
deAnnot (JSBinOpStrictNeq _) = JSBinOpStrictNeq JSNoAnnot
deAnnot (JSBinOpTimes _) = JSBinOpTimes JSNoAnnot
deAnnot (JSBinOpUrsh _) = JSBinOpUrsh JSNoAnnot

binOpEq :: JSBinOp -> JSBinOp -> Bool
binOpEq a b = deAnnot a == deAnnot b
