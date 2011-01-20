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
   , alexSetInput  
   , alexGetInput  
   , setLocation
   , getLocation
   , getInput
   , setInput
   , getLastToken
   , setLastToken
   -- , setLastEOL
   -- , getLastEOL
   , ParseError (..)
   , ParseState (..)
   , initialState
   , addComment
   , getComments
   , spanError
   , AlexInput 
   , Byte  
   ) where

import Control.Applicative ((<$>))
import Control.Monad.Error as Error
import Control.Monad.State.Class
import Control.Monad.State.Strict as State
import Language.JavaScript.Parser.ParseError (ParseError (..))
import Language.JavaScript.Parser.SrcLocation (AlexPosn (..), alexStartPos, alexSpanEmpty, Span (..))
import Language.JavaScript.Parser.Token (Token (..))
import Prelude hiding (span)
import Data.Word (Word8)

internalError :: String -> P a 
internalError = throwError . StrError 

spanError :: Span a => a -> String -> P b 
--spanError x str = throwError $ StrError $ unwords [prettyText $ getSpan x, str]
spanError x str = throwError $ StrError $ show ([show (getSpan x), str])

type Byte = Word8

type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- pending bytes on current char
                  String)       -- current input string


data ParseState = ParseState {
        alex_pos :: !AlexPosn,  -- position at current input location
        alex_inp :: String,     -- the current input
        alex_chr :: !Char,      -- the character before the input
        alex_bytes :: [Byte],
        alex_scd :: !Int        -- the current startcode
        
    , previousToken :: !Token  -- the previous token
    , comments :: [Token]      -- accumulated comments 
    }

initialState :: String -> ParseState
initialState inp 
   = ParseState 
   { alex_pos = alexStartPos
   , alex_inp = inp
   , alex_chr = '\n'          
   , alex_bytes = []             
   , alex_scd = 0               
   , previousToken = initToken
   , comments = []
   }
{-
data ParseState = 
   ParseState 
   { location :: !SrcLocation -- position at current input location
   -- , input :: !String         -- the current input
   , input :: !AlexInput      -- the current input     
   , previousToken :: !Token  -- the previous token
   -- , lastEOL :: !SrcSpan      -- location of the most recent end-of-line encountered
   , comments :: [Token]      -- accumulated comments 
   }
   deriving Show
-}

initToken :: Token
--initToken = NewlineToken SpanEmpty 
initToken = CommentToken alexSpanEmpty ""



type P a = StateT ParseState (Either ParseError) a

execParser :: P a -> ParseState -> Either ParseError a
execParser = evalStateT 

execParserKeepComments :: P a -> ParseState -> Either ParseError (a, [Token])
execParserKeepComments parser astate = 
   evalStateT (parser >>= \x -> getComments >>= \c -> return (x, c)) astate

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
{-
setLastEOL :: SrcSpan -> P ()
setLastEOL span = modify $ \s -> s { lastEOL = span }

getLastEOL :: P SrcSpan
getLastEOL = gets lastEOL
-}

alexGetInput :: P AlexInput
alexGetInput
 -- = P $ \s@ParseState{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp} -> 
 --        Right (s, (pos,c,bs,inp))
 = do
   pos <- gets alex_pos
   c   <- gets alex_chr
   bs  <- gets alex_bytes
   inp <- gets alex_inp
   return (pos,c,bs,inp)

alexSetInput :: AlexInput -> P ()
alexSetInput (pos,c,bs,inp)
 -- = P $ \s -> case s{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp} of
 --                  s@(ParseState{}) -> Right (s, ())
 = modify $ \s -> s{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp}


setLocation :: AlexPosn -> P ()
setLocation loc = modify $ \s -> s { alex_pos = loc } 

getLocation :: P AlexPosn
getLocation = gets alex_pos 

getInput :: P String 
getInput = gets alex_inp 

setInput :: String -> P ()
setInput inp = modify $ \s -> s { alex_inp = inp }

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
