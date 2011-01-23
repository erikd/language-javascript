{-# LANGUAGE DeriveDataTypeable #-}
module Language.JavaScript.Parser.AST 
       (
           Node (..)
         , JSNode(..)
         , SrcSpan (..)
         , showStripped  
       ) where

import Data.Data
import Data.List
import Language.JavaScript.Parser.SrcLocation (SrcSpan(..))

-- ---------------------------------------------------------------------

data JSNode = NS Node SrcSpan
    deriving (Show, Eq, Read, Data, Typeable)

data Node = JSArguments [[JSNode]]   
              | JSArrayLiteral [JSNode] 
              | JSBlock JSNode 
              | JSBreak [JSNode] [JSNode] 
              | JSCallExpression String [JSNode]  -- type : ., (), []; rest  
              | JSCase JSNode JSNode 
              | JSCatch JSNode [JSNode] JSNode 
              | JSContinue [JSNode] 
              | JSDecimal String  -- Was Integer   
              | JSDefault JSNode 
              | JSDoWhile JSNode JSNode JSNode 
              | JSElision [JSNode] 
              | JSEmpty JSNode 
              | JSExpression [JSNode] 
              | JSExpressionBinary String [JSNode] [JSNode] 
              | JSExpressionParen JSNode 
              | JSExpressionPostfix String [JSNode] 
              | JSExpressionTernary [JSNode] [JSNode] [JSNode] 
              | JSFinally JSNode  
              | JSFor [JSNode] [JSNode] [JSNode] JSNode 
              | JSForIn [JSNode] JSNode JSNode 
              | JSForVar [JSNode] [JSNode] [JSNode] JSNode 
              | JSForVarIn JSNode JSNode JSNode 
              | JSFunction JSNode [JSNode] JSNode  -- name, parameter list, body
              | JSFunctionBody [JSNode] 
              | JSFunctionExpression [JSNode] [JSNode] JSNode  -- name, parameter list, body                
              | JSHexInteger String  -- Was Integer  
              | JSIdentifier String 
              | JSIf JSNode JSNode 
              | JSIfElse JSNode JSNode JSNode 
              | JSLabelled JSNode JSNode  
              | JSLiteral String  
              | JSMemberDot [JSNode] JSNode 
              | JSMemberSquare [JSNode] JSNode 
              | JSObjectLiteral [JSNode]  
              | JSOperator String  
              | JSPropertyNameandValue JSNode [JSNode] 
              | JSPropertyAccessor String JSNode [JSNode] JSNode 
              | JSRegEx String 
              | JSReturn [JSNode] 
              | JSSourceElements [JSNode] 
              | JSSourceElementsTop [JSNode] 
              | JSStatementBlock JSNode 
              | JSStatementList [JSNode] 
              | JSStringLiteral Char [Char] 
              | JSSwitch JSNode [JSNode] 
              | JSThrow JSNode  
              | JSTry JSNode [JSNode]  
              | JSUnary String  
              | JSVarDecl JSNode [JSNode] 
              | JSVariables String [JSNode]  
              | JSWhile JSNode JSNode 
              | JSWith JSNode [JSNode] 
    deriving (Show, Eq, Read, Data, Typeable)

-- Strip out the location info, leaving the original JSNode text representation
showStripped :: JSNode -> String
showStripped = ss

-- Alias for internal use
ss :: JSNode -> String
ss (NS node _) = showStrippedNode node

sss :: [JSNode] -> String
--sss xs = "[" ++ (concatMap ss xs) ++ "]"
sss xs = "[" ++ (concat (intersperse "," $ map ss xs)) ++ "]"

ssss :: [[JSNode]] -> String
--ssss xss = "[" ++ (concatMap sss xss) ++ "]"
ssss xss = "[" ++ (concat (intersperse "," $ map sss xss)) ++ "]"

showStrippedNode :: Node -> String
showStrippedNode (JSArguments xss) = "JSArguments " ++ ssss xss
showStrippedNode (JSArrayLiteral xs) = "JSArrayLiteral " ++ sss xs
showStrippedNode (JSBlock x) = "JSBlock (" ++ ss x ++ ")"
showStrippedNode (JSBreak x1s x2s) = "JSBreak " ++ sss x1s ++ " " ++ sss x2s
showStrippedNode (JSCallExpression s xs) = "JSCallExpression " ++ show s ++ " " ++ sss xs
showStrippedNode (JSCase x1 x2) = "JSCase (" ++ ss x1 ++ ") (" ++ ss x2 ++ ")"
showStrippedNode (JSCatch x1 x2s x3) = "JSCatch (" ++ ss x1 ++ ") " ++ sss x2s ++ " (" ++ ss x3 ++ ")"
showStrippedNode (JSContinue xs) = "JSContinue " ++ sss xs
showStrippedNode (JSDecimal s) = "JSDecimal " ++ show s
showStrippedNode (JSDefault x) = "JSDefault (" ++ ss x ++ ")"
showStrippedNode (JSDoWhile x1 x2 x3) = "JSDoWhile (" ++ ss x1 ++ ") (" ++ ss x2 ++ ") (" ++ ss x3 ++ ")"
showStrippedNode (JSElision xs) = "JSElision " ++ sss xs
showStrippedNode (JSEmpty x) = "JSEmpty (" ++ ss x ++ ")"
showStrippedNode (JSExpression xs) = "JSExpression " ++ sss xs
showStrippedNode (JSExpressionBinary s x2s x3s) = "JSExpressionBinary " ++ show s ++ " " ++ sss x2s ++ " " ++ sss x3s
showStrippedNode (JSExpressionParen x) = "JSExpressionParen (" ++ ss x ++ ")"
showStrippedNode (JSExpressionPostfix s xs) = "JSExpressionPostfix " ++ show s ++ " " ++ sss xs
showStrippedNode (JSExpressionTernary x1s x2s x3s) = "JSExpressionTernary " ++ sss x1s ++ " " ++ sss x2s ++ " " ++ sss x3s
showStrippedNode (JSFinally x) = "JSFinally (" ++ ss x ++ ")"
showStrippedNode (JSFor x1s x2s x3s x4) = "JSFor " ++ sss x1s ++ " " ++ sss x2s ++ " " ++ sss x3s ++ " (" ++ ss x4 ++ ")"
showStrippedNode (JSForIn x1s x2 x3) = "JSForIn " ++ sss x1s ++ " (" ++ ss x2 ++ ") (" ++ ss x3 ++ ")"
showStrippedNode (JSForVar x1s x2s x3s x4) = "JSForVar " ++ sss x1s ++ " " ++ sss x2s ++ " " ++ sss x3s ++ " (" ++ ss x4 ++ ")"
showStrippedNode (JSForVarIn x1 x2 x3) = "JSForVarIn (" ++ ss x1 ++ ") (" ++ ss x2 ++ ") (" ++ ss x3 ++ ")"
showStrippedNode (JSFunction x1 x2s x3) = "JSFunction (" ++ ss x1 ++ ") " ++ sss x2s ++ " (" ++ ss x3 ++ ")"
showStrippedNode (JSFunctionBody xs) = "JSFunctionBody " ++ sss xs
showStrippedNode (JSFunctionExpression x1s x2s x3) = "JSFunctionExpression " ++ sss x1s ++ " " ++ sss x2s ++ " (" ++ ss x3 ++ ")"
showStrippedNode (JSHexInteger s) = "JSHexInteger " ++ show s
showStrippedNode (JSIdentifier s) = "JSIdentifier " ++ show s
showStrippedNode (JSIf x1 x2) = "JSIf (" ++ ss x1 ++ ") (" ++ ss x2 ++ ")"
showStrippedNode (JSIfElse x1 x2 x3) = "JSIfElse (" ++ ss x1 ++ ") (" ++ ss x2 ++ ") (" ++ ss x3 ++ ")"
showStrippedNode (JSLabelled x1 x2) = "JSLabelled (" ++ ss x1 ++ ") (" ++ ss x2 ++ ")"
showStrippedNode (JSLiteral s) = "JSLiteral " ++ show s
showStrippedNode (JSMemberDot x1s x2) = "JSMemberDot " ++ sss x1s ++ " (" ++ ss x2 ++ ")"
showStrippedNode (JSMemberSquare x1s x2) = "JSMemberSquare " ++ sss x1s ++ " (" ++ ss x2 ++ ")"
showStrippedNode (JSObjectLiteral xs) = "JSObjectLiteral " ++ sss xs
showStrippedNode (JSOperator s) = "JSOperator " ++ show s
showStrippedNode (JSPropertyNameandValue x1 x2s) = "JSPropertyNameandValue (" ++ ss x1 ++ ") " ++ sss x2s
showStrippedNode (JSPropertyAccessor s x1 x2s x3) = "JSPropertyAccessor " ++ show s ++ " (" ++ ss x1 ++ ") " ++ sss x2s ++ " (" ++ ss x3 ++ ")"
showStrippedNode (JSRegEx s) = "JSRegEx " ++ show s
showStrippedNode (JSReturn xs) = "JSReturn " ++ sss xs
showStrippedNode (JSSourceElements xs) = "JSSourceElements " ++ sss xs
showStrippedNode (JSSourceElementsTop xs) = "JSSourceElementsTop " ++ sss xs
showStrippedNode (JSStatementBlock x) = "JSStatementBlock (" ++ ss x ++ ")"
showStrippedNode (JSStatementList xs) = "JSStatementList " ++ sss xs
showStrippedNode (JSStringLiteral c s) = "JSStringLiteral " ++ show c ++ " " ++ show s
showStrippedNode (JSSwitch x x2s) = "JSSwitch (" ++ ss x ++ ") " ++ sss x2s
showStrippedNode (JSThrow x) = "JSThrow (" ++ ss x ++ ")"
showStrippedNode (JSTry x1 x2s) = "JSTry (" ++ ss x1 ++ ") " ++ sss x2s
showStrippedNode (JSUnary s) = "JSUnary " ++ show s
showStrippedNode (JSVarDecl x1 x2s) = "JSVarDecl (" ++ ss x1 ++ ") " ++ sss x2s
showStrippedNode (JSVariables s xs) = "JSVariables " ++ show s ++ " " ++ sss xs
showStrippedNode (JSWhile x1 x2) = "JSWhile (" ++ ss x1 ++ ") (" ++ ss x2 ++ ")"
showStrippedNode (JSWith x1 x2s) = "JSWith (" ++ ss x1 ++ ") " ++ sss x2s

-- EOF
