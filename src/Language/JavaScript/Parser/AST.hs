{-# LANGUAGE DeriveDataTypeable #-}
module Language.JavaScript.Parser.AST
       (
           Node (..)
         , JSNode(..)
         -- , TokenPosn (..)
         , showStripped
       ) where

import Data.Data
import Data.List
import Language.JavaScript.Parser.SrcLocation (TokenPosn(..))
import Language.JavaScript.Parser.Token

-- ---------------------------------------------------------------------

-- |The JSNode is the building block of the AST.
-- Each has a syntactic part 'Node', the position of the first leaf element
-- 'TokenPosn', as well as an array (length 0 or 1) of comments that were collected
-- while parsing. The comments will only decorate leaf elements.
data JSNode = NS Node TokenPosn [CommentAnnotation]
    deriving (Show, Eq, Read, Data, Typeable)

data Node = JSArguments JSNode [JSNode] JSNode -- lb, args, rb
              | JSArrayLiteral JSNode [JSNode] JSNode
              | JSBlock [JSNode] JSNode [JSNode] -- optional lb,block,optional rb
              | JSBreak [JSNode] [JSNode]
              | JSCallExpression String [JSNode] [JSNode] [JSNode]  -- type : ., (), []; opening [ or ., contents, closing
              | JSCase JSNode JSNode JSNode JSNode -- case,expr,colon,stmtlist
              | JSCatch JSNode JSNode JSNode [JSNode] JSNode JSNode -- catch,lb,ident,[if,expr],rb,block
              | JSContinue [JSNode]
              | JSDecimal String  -- Was Integer
              | JSDefault JSNode JSNode JSNode -- default,colon,stmtlist
              | JSDoWhile JSNode JSNode JSNode JSNode JSNode JSNode JSNode -- do,stmt,while,lb,expr,rb,autosemi
              | JSElision [JSNode]
              | JSEmpty JSNode
              | JSExpression [JSNode]
              | JSExpressionBinary String [JSNode] JSNode [JSNode] -- what, lhs, op, rhs
              | JSExpressionParen JSNode JSNode JSNode -- Literals for the parentheses, to keep posn and comments
              | JSExpressionPostfix String [JSNode] JSNode -- type, expression, operator
              | JSExpressionTernary [JSNode] JSNode [JSNode] JSNode [JSNode] -- cond, ?, trueval, :, falseval
              | JSFinally JSNode JSNode -- finally,block
              | JSFor JSNode JSNode [JSNode] JSNode [JSNode] JSNode [JSNode] JSNode JSNode -- for,lb,expr,semi,expr,semi,expr,rb.stmt
              | JSForIn JSNode JSNode [JSNode] JSNode JSNode JSNode JSNode -- for,lb,expr,in,expr,rb,stmt
              | JSForVar JSNode JSNode JSNode [JSNode] JSNode [JSNode] JSNode [JSNode] JSNode JSNode -- for,lb,var,vardecl,semi,expr,semi,expr,rb,stmt
              | JSForVarIn JSNode JSNode JSNode JSNode JSNode JSNode JSNode JSNode -- for,lb,var,vardecl,in,expr,rb,stmt
              | JSFunction JSNode JSNode JSNode [JSNode] JSNode JSNode JSNode JSNode  -- fn,name, lb,parameter list,rb,lb,body,rb
              | JSFunctionBody [JSNode]
              | JSFunctionExpression JSNode [JSNode] JSNode [JSNode] JSNode JSNode JSNode JSNode  -- fn,[name],lb, parameter list,rb,lb, body,rb
              | JSHexInteger String  -- Was Integer
              | JSIdentifier String
              | JSIf     JSNode JSNode JSNode JSNode JSNode        -- if,(,expr,),stmt
              | JSIfElse JSNode JSNode JSNode JSNode JSNode JSNode -- if,(,expr,),stmt,rest
              | JSLabelled JSNode JSNode JSNode -- identifier,colon,stmt
              | JSLiteral String
              | JSMemberDot [JSNode] JSNode JSNode -- firstpart, dot, name
              | JSMemberSquare [JSNode] JSNode JSNode JSNode -- firstpart, lb, expr, rb
              | JSObjectLiteral JSNode [JSNode] JSNode -- lbrace contents rbrace
              | JSOperator JSNode -- opnode
              | JSPropertyNameandValue JSNode JSNode [JSNode] -- name, colon, value
              -- | JSPropertyAccessor String JSNode [JSNode] JSNode -- (get|set), name, params, functionbody
              | JSPropertyAccessor JSNode JSNode JSNode [JSNode] JSNode JSNode JSNode JSNode -- (get|set), name, lb, params, rb, lb, functionbody, rb
              | JSRegEx String
              | JSReturn [JSNode]
              | JSSourceElements [JSNode]
              | JSSourceElementsTop [JSNode]
              | JSStatementBlock JSNode JSNode JSNode -- lb,block,rb
              | JSStatementList [JSNode]
              | JSStringLiteral Char [Char]
              | JSSwitch JSNode JSNode JSNode JSNode [JSNode] -- switch,lb,expr,rb,stmt
              | JSThrow JSNode JSNode -- throw val
              | JSTry JSNode JSNode [JSNode] -- try,block,rest
              | JSUnary String JSNode -- type, operator
              | JSVarDecl JSNode [JSNode]
              | JSVariables JSNode [JSNode] JSNode -- var|const, decl, autosemi
              | JSWhile JSNode JSNode JSNode JSNode JSNode -- while,lb,expr,rb,stmt
              | JSWith JSNode JSNode JSNode JSNode [JSNode] -- with,lb,expr,rb,stmt list
    deriving (Show, Eq, Read, Data, Typeable)

-- Strip out the location info, leaving the original JSNode text representation
showStripped :: JSNode -> String
showStripped = ss

-- Alias for internal use
ss :: JSNode -> String
ss (NS node _ _) = showStrippedNode node

sss :: [JSNode] -> String
--sss xs = "[" ++ (concatMap ss xs) ++ "]"
sss xs = "[" ++ (concat (intersperse "," $ map ss xs)) ++ "]"

ssss :: [[JSNode]] -> String
--ssss xss = "[" ++ (concatMap sss xss) ++ "]"
ssss xss = "[" ++ (concat (intersperse "," $ map sss xss)) ++ "]"

showStrippedNode :: Node -> String
showStrippedNode (JSArguments lb xs rb) = "JSArguments " ++ sss xs
showStrippedNode (JSArrayLiteral lb xs rb) = "JSArrayLiteral " ++ sss xs
showStrippedNode (JSBlock lb x rb) = "JSBlock (" ++ ss x ++ ")"
showStrippedNode (JSBreak x1s x2s) = "JSBreak " ++ sss x1s ++ " " ++ sss x2s
showStrippedNode (JSCallExpression s os xs cs) = "JSCallExpression " ++ show s ++ " " ++ sss xs
showStrippedNode (JSCase ca x1 c x2) = "JSCase (" ++ ss x1 ++ ") (" ++ ss x2 ++ ")"
showStrippedNode (JSCatch c lb x1 x2s rb x3) = "JSCatch (" ++ ss x1 ++ ") " ++ sss x2s ++ " (" ++ ss x3 ++ ")"
showStrippedNode (JSContinue xs) = "JSContinue " ++ sss xs
showStrippedNode (JSDecimal s) = "JSDecimal " ++ show s
showStrippedNode (JSDefault d c x) = "JSDefault (" ++ ss x ++ ")"
showStrippedNode (JSDoWhile d x1 w lb x2 rb x3) = "JSDoWhile (" ++ ss x1 ++ ") (" ++ ss x2 ++ ") (" ++ ss x3 ++ ")"
showStrippedNode (JSElision xs) = "JSElision " ++ sss xs
showStrippedNode (JSEmpty x) = "JSEmpty (" ++ ss x ++ ")"
showStrippedNode (JSExpression xs) = "JSExpression " ++ sss xs
showStrippedNode (JSExpressionBinary s x2s op x3s) = "JSExpressionBinary " ++ show s ++ " " ++ sss x2s ++ " " ++ sss x3s
showStrippedNode (JSExpressionParen lp x rp) = "JSExpressionParen (" ++ ss x ++ ")"
showStrippedNode (JSExpressionPostfix s xs op) = "JSExpressionPostfix " ++ show s ++ " " ++ sss xs
showStrippedNode (JSExpressionTernary x1s q x2s c x3s) = "JSExpressionTernary " ++ sss x1s ++ " " ++ sss x2s ++ " " ++ sss x3s
showStrippedNode (JSFinally f x) = "JSFinally (" ++ ss x ++ ")"
showStrippedNode (JSFor f lb x1s s1 x2s s2 x3s rb x4) = "JSFor " ++ sss x1s ++ " " ++ sss x2s ++ " " ++ sss x3s ++ " (" ++ ss x4 ++ ")"
showStrippedNode (JSForIn f lb x1s i x2 rb x3) = "JSForIn " ++ sss x1s ++ " (" ++ ss x2 ++ ") (" ++ ss x3 ++ ")"
showStrippedNode (JSForVar f lb v x1s s1 x2s s2 x3s rb x4) = "JSForVar " ++ sss x1s ++ " " ++ sss x2s ++ " " ++ sss x3s ++ " (" ++ ss x4 ++ ")"
showStrippedNode (JSForVarIn f lb v x1 i x2 rb x3) = "JSForVarIn (" ++ ss x1 ++ ") (" ++ ss x2 ++ ") (" ++ ss x3 ++ ")"
showStrippedNode (JSFunction f x1 lb x2s rb lb2 x3 rb2) = "JSFunction (" ++ ss x1 ++ ") " ++ sss x2s ++ " (" ++ ss x3 ++ ")"
showStrippedNode (JSFunctionBody xs) = "JSFunctionBody " ++ sss xs
showStrippedNode (JSFunctionExpression f x1s lb x2s rb lb2 x3 rb2) = "JSFunctionExpression " ++ sss x1s ++ " " ++ sss x2s ++ " (" ++ ss x3 ++ ")"
showStrippedNode (JSHexInteger s) = "JSHexInteger " ++ show s
showStrippedNode (JSIdentifier s) = "JSIdentifier " ++ show s
showStrippedNode (JSIf i lb x1 rb x2) = "JSIf (" ++ ss x1 ++ ") (" ++ ss x2 ++ ")"
showStrippedNode (JSIfElse i lb x1 rb x2 x3) = "JSIfElse (" ++ ss x1 ++ ") (" ++ ss x2 ++ ") (" ++ ss x3 ++ ")"
showStrippedNode (JSLabelled x1 c x2) = "JSLabelled (" ++ ss x1 ++ ") (" ++ ss x2 ++ ")"
showStrippedNode (JSLiteral s) = "JSLiteral " ++ show s
showStrippedNode (JSMemberDot x1s d x2 ) = "JSMemberDot " ++ sss x1s ++ " (" ++ ss x2 ++ ")"
showStrippedNode (JSMemberSquare x1s lb x2 rb) = "JSMemberSquare " ++ sss x1s ++ " (" ++ ss x2 ++ ")"
showStrippedNode (JSObjectLiteral lb xs rb) = "JSObjectLiteral " ++ sss xs
showStrippedNode (JSOperator n) = "JSOperator " ++ ss n
showStrippedNode (JSPropertyNameandValue x1 colon x2s) = "JSPropertyNameandValue (" ++ ss x1 ++ ") " ++ sss x2s
showStrippedNode (JSPropertyAccessor s x1 lb1 x2s rb1 lb2 x3 rb2) = "JSPropertyAccessor " ++ show s ++ " (" ++ ss x1 ++ ") " ++ sss x2s ++ " (" ++ ss x3 ++ ")"
showStrippedNode (JSRegEx s) = "JSRegEx " ++ show s
showStrippedNode (JSReturn xs) = "JSReturn " ++ sss xs
showStrippedNode (JSSourceElements xs) = "JSSourceElements " ++ sss xs
showStrippedNode (JSSourceElementsTop xs) = "JSSourceElementsTop " ++ sss xs
showStrippedNode (JSStatementBlock lb x rb) = "JSStatementBlock (" ++ ss x ++ ")"
showStrippedNode (JSStatementList xs) = "JSStatementList " ++ sss xs
showStrippedNode (JSStringLiteral c s) = "JSStringLiteral " ++ show c ++ " " ++ show s
showStrippedNode (JSSwitch s lb x rb x2s) = "JSSwitch (" ++ ss x ++ ") " ++ sss x2s
showStrippedNode (JSThrow t x) = "JSThrow (" ++ ss x ++ ")"
showStrippedNode (JSTry t x1 x2s) = "JSTry (" ++ ss x1 ++ ") " ++ sss x2s
showStrippedNode (JSUnary s x) = "JSUnary " ++ show s
showStrippedNode (JSVarDecl x1 x2s) = "JSVarDecl (" ++ ss x1 ++ ") " ++ sss x2s
showStrippedNode (JSVariables n xs as) = "JSVariables " ++ ss n ++ " " ++ sss xs
showStrippedNode (JSWhile w lb x1 rb x2) = "JSWhile (" ++ ss x1 ++ ") (" ++ ss x2 ++ ")"
showStrippedNode (JSWith w lb x1 rb x2s) = "JSWith (" ++ ss x1 ++ ") " ++ sss x2s

-- EOF
