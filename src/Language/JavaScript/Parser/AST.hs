{-# LANGUAGE DeriveDataTypeable #-}
module Language.JavaScript.Parser.AST 
       (
           JSNode(..)
         , NodeSpan (..)
       ) where

import Data.Data
import Language.JavaScript.Parser.SrcLocation (SrcSpan(..))

-- ---------------------------------------------------------------------

data NodeSpan = NS JSNode SrcSpan

data JSNode = JSArguments [[JSNode]]  
              | JSArrayLiteral [JSNode]
              | JSBlock JSNode
              | JSBreak [JSNode] [JSNode]
              | JSCallExpression String [JSNode] -- type : ., (), []; rest  
              | JSCase JSNode JSNode
              | JSCatch JSNode [JSNode] JSNode
              | JSContinue [JSNode]
              | JSDecimal String -- Was Integer   
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
              | JSFunction JSNode [JSNode] JSNode -- name, parameter list, body
              | JSFunctionBody [JSNode]
              | JSFunctionExpression [JSNode] [JSNode] JSNode -- name, parameter list, body                
              | JSHexInteger String -- Was Integer  
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

