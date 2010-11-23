{-# OPTIONS  #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Python.Common.ParserMonad 
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Monad support for Python parser and lexer. 
-----------------------------------------------------------------------------

module Language.JavaScript.Parser.ParserMonad 
   ( P
   , execParser
   , execParserKeepComments
   , runParser
   , thenP
   , returnP
   , setLocation
   , getLocation
   , getInput
   , setInput
   , getLastToken
   , setLastToken
   , setLastEOL
   , getLastEOL
   , ParseError (..)
   , ParseState (..)
   , initialState
   , pushStartCode
   , popStartCode
   , getStartCode
   , getIndent
   , pushIndent
   , popIndent
   , getIndentStackDepth
   , getParen
   , pushParen
   , popParen
   , getParenStackDepth
   , addComment
   , getComments
   , spanError
   ) where

import Language.JavaScript.Parser.SrcLocation (SrcLocation (..), SrcSpan (..), Span (..))
import Language.JavaScript.Parser.Token (Token (..))
import Language.JavaScript.Parser.ParseError (ParseError (..))
import Control.Applicative ((<$>))
import Control.Monad.State.Class
import Control.Monad.State.Strict as State
import Control.Monad.Error as Error
import Control.Monad.Error.Class
import Control.Monad.Identity as Identity
import Control.Monad.Trans as Trans
--import Language.JavaScript.Parser.Pretty

internalError :: String -> P a 
internalError = throwError . StrError 

spanError :: Span a => a -> String -> P b 
--spanError x str = throwError $ StrError $ unwords [prettyText $ getSpan x, str]
spanError x str = throwError $ StrError $ show ([show (getSpan x), str])

data ParseState = 
   ParseState 
   { location :: !SrcLocation -- position at current input location
   , input :: !String         -- the current input
   , previousToken :: !Token  -- the previous token
   , startCodeStack :: [Int]  -- a stack of start codes for the state of the lexer
   , indentStack :: [Int]     -- a stack of source column positions of indentation levels
   , parenStack :: [Token]    -- a stack of parens and brackets for indentation handling
   , lastEOL :: !SrcSpan      -- location of the most recent end-of-line encountered
   , comments :: [Token]      -- accumulated comments 
   }
   deriving Show

initToken :: Token
initToken = NewlineToken SpanEmpty 

initialState :: SrcLocation -> String -> [Int] -> ParseState
initialState initLoc inp scStack
   = ParseState 
   { location = initLoc 
   , input = inp
   , previousToken = initToken
   , startCodeStack = scStack
   , indentStack = [1]
   , parenStack = []
   , lastEOL = SpanEmpty 
   , comments = []
   }

type P a = StateT ParseState (Either ParseError) a

execParser :: P a -> ParseState -> Either ParseError a
execParser = evalStateT 

execParserKeepComments :: P a -> ParseState -> Either ParseError (a, [Token])
execParserKeepComments parser state = 
   evalStateT (parser >>= \x -> getComments >>= \c -> return (x, c)) state

runParser :: P a -> ParseState -> Either ParseError (a, ParseState)
runParser = runStateT 

{-# INLINE returnP #-}
returnP :: a -> P a
returnP = return 

{-# INLINE thenP #-}
thenP :: P a -> (a -> P b) -> P b
thenP = (>>=)

{-
failP :: SrcSpan -> [String] -> P a
failP span strs = throwError (prettyText span ++ ": " ++ unwords strs) 
-}

setLastEOL :: SrcSpan -> P ()
setLastEOL span = modify $ \s -> s { lastEOL = span }

getLastEOL :: P SrcSpan
getLastEOL = gets lastEOL

setLocation :: SrcLocation -> P ()
setLocation loc = modify $ \s -> s { location = loc } 

getLocation :: P SrcLocation
getLocation = gets location 

getInput :: P String 
getInput = gets input 

setInput :: String -> P ()
setInput inp = modify $ \s -> s { input = inp }

getLastToken :: P Token
getLastToken = gets previousToken 

setLastToken :: Token -> P ()
setLastToken tok = modify $ \s -> s { previousToken = tok } 

pushStartCode :: Int -> P () 
pushStartCode code = do
   oldStack <- gets startCodeStack
   modify $ \s -> s { startCodeStack = code : oldStack }

popStartCode :: P ()
popStartCode = do
   oldStack <- gets startCodeStack
   case oldStack of
     [] -> internalError "fatal error in lexer: attempt to pop empty start code stack"
     _:rest -> modify $ \s -> s { startCodeStack = rest }

getStartCode :: P Int
getStartCode = do 
   oldStack <- gets startCodeStack
   case oldStack of
     [] -> internalError "fatal error in lexer: start code stack empty on getStartCode"
     code:_ -> return code 

pushIndent :: Int -> P () 
pushIndent indent = do 
   oldStack <- gets indentStack
   modify $ \s -> s { indentStack = indent : oldStack }

popIndent :: P ()
popIndent = do 
   oldStack <- gets indentStack
   case oldStack of
     [] -> internalError "fatal error in lexer: attempt to pop empty indentation stack"
     _:rest -> modify $ \s -> s { indentStack = rest }

getIndent :: P Int
getIndent = do
   oldStack <- gets indentStack 
   case oldStack of
     [] -> internalError "fatal error in lexer: indent stack empty on getIndent"
     indent:_ -> return indent 

getIndentStackDepth :: P Int
getIndentStackDepth = gets (length . indentStack)

pushParen :: Token -> P () 
pushParen symbol = do
   oldStack <- gets parenStack 
   modify $ \s -> s { parenStack = symbol : oldStack }

popParen :: P ()
popParen = do
   oldStack <- gets parenStack
   case oldStack of
      [] -> internalError "fatal error in lexer: attempt to pop empty paren stack"
      _:rest -> modify $ \s -> s { parenStack = rest }  

getParen :: P (Maybe Token)
getParen = do
   oldStack <- gets parenStack
   case oldStack of
      [] -> return Nothing 
      symbol:_ -> return $ Just symbol

getParenStackDepth :: P Int
getParenStackDepth = gets (length . parenStack) 

addComment :: Token -> P ()
addComment c = do
   oldComments <- gets comments
   modify $ \s -> s { comments = c : oldComments }

getComments :: P [Token]
getComments = reverse <$> gets comments
