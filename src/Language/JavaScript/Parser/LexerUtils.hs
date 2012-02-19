-----------------------------------------------------------------------------
-- |
-- Module      : Language.JavaScript.LexerUtils
-- Based on language-python version by Bernie Pope
-- Copyright   : (c) 2009 Bernie Pope
-- License     : BSD-style
-- Stability   : experimental
-- Portability : ghc
--
-- Various utilities to support the JavaScript lexer.
-----------------------------------------------------------------------------

module Language.JavaScript.Parser.LexerUtils (
  StartCode
  -- , AlexInput
  -- , alexGetChar
  -- , alexInputPrevChar
  , symbolToken
  , mkString
  , commentToken
  , regExToken
  , decimalToken
  -- , endOfLine
  , endOfFileToken
  , assignToken
  , hexIntegerToken
  , stringToken
  --, lexicalError
  ) where

--import Control.Monad (liftM)
--import Control.Monad.Error.Class (throwError)
import Language.JavaScript.Parser.Token as Token
--import Language.JavaScript.Parser.ParserMonad
import Language.JavaScript.Parser.SrcLocation
import Prelude hiding (span)

-- Functions for building tokens

type StartCode = Int

symbolToken :: (Monad m) => (t -> a) -> t -> t1 -> t2 -> m a
--symbolToken :: (Monad m) => (TokenPosn -> Token) -> t -> t1 -> t2 -> m Token
symbolToken mkToken location _ _ = return (mkToken location)
--symbolToken mkToken location = return (mkToken location)

-- special tokens for the end of file and end of line
endOfFileToken :: Token
endOfFileToken = EOFToken tokenPosnEmpty

mkString
  :: (Monad m) => (TokenPosn -> String -> Token) -> TokenPosn -> Int -> String -> m Token
mkString toToken loc len str = do return (toToken loc (take len str))

--mkString
--  :: (Monad m) => ((t,Char,String) -> String -> Token) -> (t,Char,String) -> Int -> m Token
--mkString toToken loc@(p,c,str) len = do
--   return $ toToken loc (take len str)


{-
mkStringRemoveContinuation
  :: (Monad m) => (TokenPosn -> String -> Token) -> TokenPosn -> Int -> String -> m Token
  -- :: (Monad m) => (t -> [a1] -> a) -> t -> Int -> [a1] -> m a
mkStringRemoveContinuation toToken loc len str = do
   -- token <- toToken loc (take len str)
   return $ toToken loc (take len str)
-}

decimalToken :: TokenPosn -> String -> Token
decimalToken loc str = DecimalToken loc str

hexIntegerToken :: TokenPosn -> String -> Token
hexIntegerToken loc str = HexIntegerToken loc str

assignToken :: TokenPosn -> String -> Token
assignToken loc str = AssignToken loc str

regExToken :: TokenPosn -> String -> Token
regExToken loc str = RegExToken loc str

stringToken :: TokenPosn -> String -> Token
stringToken loc str = StringToken loc str1 delimiter
  where
    -- str1 = init $ tail str
    str1 = stripLineContinuations $ init $ tail str
    delimiter = head str

commentToken :: TokenPosn -> String -> Token
commentToken loc str = CommentToken loc str

-- ---------------------------------------------------------------------
-- Strip out any embedded line continuations
-- Recognise by \ followed by $lf | $cr | $ls | $ps | $cr $lf
-- $ls = \x2028, $ps = \x2029
stripLineContinuations :: String -> String
stripLineContinuations xs = doStripLineContinuations [] [] xs

doStripLineContinuations :: String -> String -> String -> String
doStripLineContinuations acc matched xs
  | xs == []      = acc -- Assume we are passed well-formed strings, should not be a dangling match
  | matched == [] = if (head xs == '\\')
                        then doStripLineContinuations acc ['\\'] (tail xs)
                        else doStripLineContinuations (acc ++ [head xs]) [] (tail xs)
  | otherwise = if ((head xs == '\n') || (head xs == '\r') || (head xs == '\x2028') || (head xs == '\x2029'))
                        then doStripLineContinuations acc (matched++[head xs]) (tail xs)
                        else (if (matched == ['\\'])
                                 then doStripLineContinuations (acc++matched ++ [head xs]) [] (tail xs)
                                 else doStripLineContinuations (acc++[head xs]) [] (tail xs))

-- -----------------------------------------------------------------------------
-- Functionality required by Alex
{-
lexicalError :: P a
lexicalError = do
  location <- getLocation
  c <- liftM head getInput
  -- (_,c,_,_) <- getInput
  throwError $ UnexpectedChar c location
-}
-- EOF
