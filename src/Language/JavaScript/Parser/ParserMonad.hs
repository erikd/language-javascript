{-# OPTIONS  #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.JavaScript.ParserMonad 
-- Based on language-python version by Bernie Pope
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Stability   : experimental
-- Portability : ghc
--
-- Monad support for JavaScript parser and lexer. 
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
   , addComment
   , getComments
   , spanError
   ) where

import Control.Applicative ((<$>))
import Control.Monad.Error as Error
import Control.Monad.State.Class
import Control.Monad.State.Strict as State
import Language.JavaScript.Parser.ParseError (ParseError (..))
import Language.JavaScript.Parser.SrcLocation (SrcLocation (..), SrcSpan (..), Span (..))
import Language.JavaScript.Parser.Token (Token (..))
import Prelude hiding (span)

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
   , lastEOL :: !SrcSpan      -- location of the most recent end-of-line encountered
   , comments :: [Token]      -- accumulated comments 
   }
   deriving Show

initToken :: Token
--initToken = NewlineToken SpanEmpty 
initToken = CommentToken SpanEmpty ""

initialState :: SrcLocation -> String -> [Int] -> ParseState
initialState initLoc inp scStack
   = ParseState 
   { location = initLoc 
   , input = inp
   , previousToken = initToken
   -- , startCodeStack = scStack
   , lastEOL = SpanEmpty 
   , comments = []
   -- , divideId = 0 
   -- , regId = 0         
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

addComment :: Token -> P ()
addComment c = do
   oldComments <- gets comments
   modify $ \s -> s { comments = c : oldComments }

getComments :: P [Token]
getComments = reverse <$> gets comments
