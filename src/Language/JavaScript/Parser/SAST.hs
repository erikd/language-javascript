{-# LANGUAGE DeriveDataTypeable #-}
module Language.JavaScript.Parser.SAST
       (
           JSNode (..)
         , fromAST
       ) where

import Data.Data
import Data.List
import qualified Language.JavaScript.Parser.AST as AST

-- ---------------------------------------------------------------------

-- |The JSNode is the building block of the AST.
-- Each has a syntactic part 'Node'. In addition, the leaf elements
-- (terminals) have a position 'TokenPosn', as well as an array of comments
-- and/or whitespace that was collected while parsing.

data JSNode =
              -- | Terminals
                JSIdentifier String
              | JSDecimal String
              | JSLiteral String
              | JSHexInteger String
              | JSOctal String
              | JSStringLiteral Char String
              | JSRegEx String

              -- | Non Terminals

              | JSArguments  [JSNode]    -- args
              | JSArrayLiteral  [JSNode] -- ^contents
              | JSBlock  [JSNode]      -- ^optional block statements
              | JSBreak  [JSNode]        -- ^optional identifier
              | JSCallExpression  String [JSNode]  -- ^type : ., (), []; contents
              | JSCase  JSNode [JSNode]    -- ^expr,stmtlist
              | JSCatch  JSNode [JSNode] JSNode -- ^ ident,expr,block
              | JSContinue  [JSNode]      -- ^optional identifier
              | JSDefault  [JSNode] -- ^stmtlist
              | JSDoWhile  JSNode JSNode  -- ^stmt,expr
              | JSElision           -- ^
              | JSExpression  [JSNode]   -- ^expression components
              | JSExpressionAssign  [JSNode] JSNode [JSNode] -- ^lhs,assignop,rhs
              | JSExpressionBinary  String [JSNode] [JSNode] -- ^what,lhs,rhs
              | JSExpressionParen  JSNode -- ^expression
              | JSExpressionPostfix String [JSNode] -- ^type,expression
              | JSExpressionTernary  [JSNode] [JSNode] [JSNode] -- ^cond,trueval,falseval
              | JSFinally JSNode -- ^block
              | JSFor  [JSNode] [JSNode] [JSNode] JSNode -- ^expr,expr,expr,stmt
              | JSForIn  [JSNode] JSNode JSNode -- ^expr,expr,stmt
              | JSForVar  [JSNode] [JSNode] [JSNode] JSNode -- ^vardecl,expr,expr,stmt
              | JSForVarIn  JSNode JSNode JSNode -- ^vardecl,expr,stmt
              | JSFunction  JSNode [JSNode] JSNode  -- ^name,parameter list,block
              | JSFunctionExpression  [JSNode] [JSNode] JSNode  -- ^[name],parameter list,block
              | JSIf  JSNode [JSNode] [JSNode] -- ^expr,stmt,optional rest
              | JSLabelled  JSNode JSNode -- ^identifier,stmt
              | JSMemberDot  [JSNode] JSNode -- ^firstpart,name
              | JSMemberSquare  [JSNode] JSNode -- ^firstpart,expr
              | JSObjectLiteral  [JSNode] -- ^contents
              | JSOperator  JSNode -- ^opnode
              | JSPropertyAccessor JSNode JSNode [JSNode] JSNode -- ^(get|set), name,params,block
              | JSPropertyNameandValue  JSNode [JSNode] -- ^name,value
              | JSReturn  [JSNode] -- ^optional expression

              | JSSourceElementsTop  [JSNode] -- ^source elements
              | JSSwitch  JSNode JSNode -- ^expr,caseblock
              | JSThrow  JSNode -- ^val
              | JSTry  JSNode [JSNode] -- ^block,rest
              | JSUnary  String JSNode -- ^type, operator
              | JSVarDecl  JSNode [JSNode] -- ^identifier, optional initializer
              | JSVariables  JSNode [JSNode] -- ^var|const, decl
              | JSWhile  JSNode JSNode -- ^expr,stmt
              | JSWith  JSNode [JSNode] -- ^expr,stmt list
    deriving (Show, Eq, Read, Data, Typeable)

-- Strip out the location info, leaving the original JSNode text representation
fromAST :: AST.JSNode -> JSNode
fromAST = ss

ss :: AST.JSNode -> JSNode
ss (AST.JSArguments _ _lb xs _rb) = JSArguments (filter (\x -> x /= JSLiteral ",") $ sss xs)
ss (AST.JSArrayLiteral _ _lb xs _rb) = JSArrayLiteral (sss xs)
ss (AST.JSBlock _ _lb xs _rb) = JSBlock (sss xs)
ss (AST.JSBreak _ _b x1s as) = JSBreak  (sss x1s)
ss (AST.JSCallExpression _ s _os xs _cs) = JSCallExpression  s (sss xs)
ss (AST.JSCase _ _ca x1 _c x2s) = JSCase (ss x1) (sss x2s)
ss (AST.JSCatch _ _c _lb x1 x2s _rb x3) = JSCatch (ss x1) (sss x2s) (ss x3)
ss (AST.JSContinue _ _c xs as) = JSContinue (sss xs)
ss (AST.JSDecimal _ s) = JSDecimal s
ss (AST.JSDefault _ _d _c xs) = JSDefault (sss xs)
ss (AST.JSDoWhile _ _d x1 _w _lb x2 _rb _as) = JSDoWhile (ss x1) (ss x2)
ss (AST.JSElision _ c) = JSElision
ss (AST.JSExpression _ xs) = JSExpression (filter (\x -> x /= JSLiteral ",") $ sss xs)
ss (AST.JSExpressionAssign _ x2s op x3s) = JSExpressionAssign  (sss x2s) (ss op) (sss x3s)
ss (AST.JSExpressionBinary _ s x2s _op x3s) = JSExpressionBinary s (sss x2s) (sss x3s)
ss (AST.JSExpressionParen _ _lp x _rp) = JSExpressionParen (ss x)
ss (AST.JSExpressionPostfix _ s xs _op) = JSExpressionPostfix s (sss xs)
ss (AST.JSExpressionTernary _ x1s _q x2s _c x3s) = JSExpressionTernary (sss x1s) (sss x2s) (sss x3s)
ss (AST.JSFinally _ _f x) = JSFinally (ss x)
ss (AST.JSFor _ _f _lb x1s _s1 x2s _s2 x3s _rb x4) = JSFor (sss x1s) (sss x2s) (sss x3s) (ss x4)
ss (AST.JSForIn _ _f _lb x1s _i x2 _rb x3) = JSForIn(sss x1s) (ss x2) (ss x3)
ss (AST.JSForVar _ _f _lb _v x1s _s1 x2s _s2 x3s _rb x4) = JSForVar (sss x1s) (sss x2s) (sss x3s) (ss x4)
ss (AST.JSForVarIn _ _f _lb _v x1 _i x2 _rb x3) = JSForVarIn (ss x1) (ss x2) (ss x3)
ss (AST.JSFunction _ _f x1 _lb x2s _rb x3) = JSFunction (ss x1) (sss x2s) (ss x3)
ss (AST.JSFunctionExpression _ _f x1s _lb x2s _rb x3) = JSFunctionExpression (sss x1s) (sss x2s) (ss x3)
ss (AST.JSHexInteger _ s) = JSHexInteger s
ss (AST.JSOctal _ s) = JSOctal s
ss (AST.JSIdentifier _ s) = JSIdentifier s
ss (AST.JSIf _ _i _lb x1 _rb x2s x3s) = JSIf (ss x1) (sss x2s) (filter (\x -> x /= JSLiteral "else") $ sss x3s)
ss (AST.JSLabelled _ x1 _c x2) = JSLabelled (ss x1) (ss x2)
ss (AST.JSLiteral _ s) = JSLiteral s
ss (AST.JSMemberDot _ x1s _d x2 ) = JSMemberDot (sss x1s) (ss x2)
ss (AST.JSMemberSquare _ x1s _lb x2 _rb) = JSMemberSquare (sss x1s) (ss x2)
ss (AST.JSObjectLiteral _ _lb xs _rb) = JSObjectLiteral (sss xs)
ss (AST.JSOperator _ n) = JSOperator (ss n)
ss (AST.JSPropertyNameandValue _ x1 _colon x2s) = JSPropertyNameandValue (ss x1) (sss x2s)
ss (AST.JSPropertyAccessor _ s x1 _lb1 x2s _rb1 x3) = JSPropertyAccessor (ss s) (ss x1) (sss x2s) (ss x3)
ss (AST.JSRegEx _ s) = JSRegEx s
ss (AST.JSReturn _ _r xs as) = JSReturn (sss xs)
ss (AST.JSSourceElementsTop _ xs) = JSSourceElementsTop (sss xs)
ss (AST.JSStringLiteral _ c s) = JSStringLiteral c s
ss (AST.JSSwitch _ _s _lb x _rb x2) = JSSwitch (ss x) (ss x2)
ss (AST.JSThrow _ _t x) = JSThrow (ss x)
ss (AST.JSTry _ _t x1 x2s) = JSTry (ss x1) (sss x2s)
ss (AST.JSUnary _ s x) = JSUnary s (ss x)
ss (AST.JSVarDecl _ x1 x2s) = JSVarDecl (ss x1) (sss x2s)
ss (AST.JSVariables _ n xs _as) = JSVariables (ss n) (sss xs)
ss (AST.JSWhile _ _w _lb x1 _rb x2) = JSWhile (ss x1) (ss x2)
ss (AST.JSWith _ _w _lb x1 _rb x2s) = JSWith (ss x1) (sss x2s)

sss :: [AST.JSNode] -> [JSNode]
sss xs = map ss xs

-- EOF
