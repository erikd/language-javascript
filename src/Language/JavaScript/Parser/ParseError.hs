-----------------------------------------------------------------------------
-- |
-- Module      : Language.JavaScript.ParseError
-- Based on language-python version by Bernie Pope
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Stability   : experimental
-- Portability : ghc
--
-- Error values for the lexer and parser. 
-----------------------------------------------------------------------------

module Language.JavaScript.Parser.ParseError ( ParseError (..) ) where

--import Language.JavaScript.Parser.Pretty
import Language.JavaScript.Parser.SrcLocation (SrcLocation)
import Language.JavaScript.Parser.Token (Token)
import Control.Monad.Error.Class

data ParseError  
   = UnexpectedToken Token           
     -- ^ An error from the parser. Token found where it should not be. 
     --   Note: tokens contain their own source span.
   | UnexpectedChar Char SrcLocation 
     -- ^ An error from the lexer. Character found where it should not be.
   | StrError String                 
     -- ^ A generic error containing a string message. No source location.
   deriving (Eq, Ord, Show)

instance Error ParseError where
   noMsg = StrError ""
   strMsg = StrError 

