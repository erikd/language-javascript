{-# LANGUAGE CPP, DeriveDataTypeable #-}
{-# OPTIONS  #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.JavaScript.Parser.ParserMonad 
-- Copyright   : (c) 2009 Bernie Pope? Used as base.
-- License     : BSD-style
-- Maintainer  : alan.zimm@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Monad support for JavaScript parser and lexer, based on language-python version by Bernie Pope
-----------------------------------------------------------------------------

module Language.JavaScript.Parser.ParserMonad 
   ( P
   --, execParser
   --, execParserKeepComments
   --, runParser
   , thenP
   , returnP
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
   -- , initialState
   -- , pushStartCode
   -- , popStartCode
   , getStartCode
   -- , getIndent
   -- , pushIndent
   -- , popIndent
   -- , getIndentStackDepth
   -- , getParen
   -- , pushParen
   -- , popParen
   -- , getParenStackDepth
   -- , addComment
   -- , getComments
   -- , spanError
   , Token (..)  
   , endOfFileToken
   , lexicalError  
   -- -----------  
   , AlexInput(..)  
   , alexGetChar  
   , alexInputPrevChar  
   -- ----------  
   , symbolToken  
   , StartCode  
   , Action  
   ) where

--import Language.Python.Common.SrcLocation (SrcLocation (..), SrcSpan (..), Span (..))
--import Language.Python.Common.Token (Token (..))
--import Language.Python.Common.ParseError (ParseError (..))
import Language.JavaScript.Parser.SrcLocation
import Control.Applicative ((<$>))
import Control.Monad.State.Class
import Control.Monad.State.Strict as State
import Control.Monad.Error as Error
import Control.Monad.Error.Class
import Control.Monad.Identity as Identity
import Control.Monad.Trans as Trans
--import Language.Python.Common.Pretty
import Data.Data

-- The token type:

data Token
      =  TokenLet { token_span :: !SrcSpan }
      | TokenIn { token_span :: !SrcSpan }
      | TokenInt {-Int-} { token_span :: !SrcSpan }
      | TokenVar {-String-} { token_span :: !SrcSpan }
      | TokenEq { token_span :: !SrcSpan }
      | TokenPlus { token_span :: !SrcSpan }
      | TokenMinus { token_span :: !SrcSpan }
      | TokenTimes { token_span :: !SrcSpan }
      | TokenDiv { token_span :: !SrcSpan }
      | TokenOB { token_span :: !SrcSpan }
      | TokenCB { token_span :: !SrcSpan }
      -- Special cases
      | EOFToken { token_span :: !SrcSpan }                          -- ^ End of file 
 deriving (Eq,Ord,Show{-,Typeable,Data-})


endOfFileToken :: Token
--endOfFileToken = EOFToken SpanEmpty
endOfFileToken = EOFToken SpanEmpty

{-
-- | Source location spanning a contiguous section of a file.
data SrcSpan
    -- | A span which starts and ends on the same line.
  = SpanCoLinear
    { span_filename     :: !String
    , span_row          :: {-# UNPACK #-} !Int
    , span_start_column :: {-# UNPACK #-} !Int
    , span_end_column   :: {-# UNPACK #-} !Int
    }
    -- | A span which starts and ends on different lines.
  | SpanMultiLine
    { span_filename     :: !String
    , span_start_row    :: {-# UNPACK #-} !Int
    , span_start_column :: {-# UNPACK #-} !Int
    , span_end_row      :: {-# UNPACK #-} !Int
    , span_end_column   :: {-# UNPACK #-} !Int
    }
    -- | A span which is actually just one point in the file.
  | SpanPoint
    { span_filename :: !String
    , span_row      :: {-# UNPACK #-} !Int
    , span_column   :: {-# UNPACK #-} !Int
    }
    -- | No span information.
  | SpanEmpty 
   deriving (Eq,Ord,Show,Typeable,Data)
-}

data ParseError  
   = UnexpectedToken Token           -- ^ An error from the parser. Token found where it should not be. Note: tokens contain their own source span.
   | UnexpectedChar Char SrcLocation -- ^ An error from the lexer. Character found where it should not be.
   | StrError String                 -- ^ A generic error containing a string message. No source location.
   deriving (Eq, Ord, Show)

instance Error ParseError where
   noMsg = StrError ""
   strMsg = StrError 


internalError :: String -> P a 
internalError = throwError . StrError 
{-
spanError :: Span a => a -> String -> P b 
spanError x str = throwError $ StrError $ unwords [prettyText $ getSpan x, str]
-}

data ParseState = 
   ParseState 
   { location :: !SrcLocation -- position at current input location
   , input :: !String         -- the current input
   , previousToken :: !Token  -- the previous token
   , startCodeStack :: [Int]  -- a stack of start codes for the state of the lexer
   , indentStack :: [Int]     -- a stack of source column positions of indentation levels
   , parenStack :: [Token]    -- a stack of parens and brackets for indentation handling
   -- , lastEOL :: !SrcSpan      -- location of the most recent end-of-line encountered
   , comments :: [Token]      -- accumulated comments 
   }
   deriving Show

{-
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
-}
type P a = StateT ParseState (Either ParseError) a

{-
execParser :: P a -> ParseState -> Either ParseError a
execParser = evalStateT 

execParserKeepComments :: P a -> ParseState -> Either ParseError (a, [Token])
execParserKeepComments parser state = 
   evalStateT (parser >>= \x -> getComments >>= \c -> return (x, c)) state
-}
runParser :: P a -> ParseState -> Either ParseError (a, ParseState)
runParser = runStateT 

{-# INLINE returnP #-}
returnP :: a -> P a
returnP = return 

{-# INLINE thenP #-}
thenP :: P a -> (a -> P b) -> P b
thenP = (>>=)
{-
{-
failP :: SrcSpan -> [String] -> P a
failP span strs = throwError (prettyText span ++ ": " ++ unwords strs) 
-}

setLastEOL :: SrcSpan -> P ()
setLastEOL span = modify $ \s -> s { lastEOL = span }

getLastEOL :: P SrcSpan
getLastEOL = gets lastEOL
-}
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
{-
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
-}
getStartCode :: P Int
getStartCode = do 
   oldStack <- gets startCodeStack
   case oldStack of
     [] -> internalError "fatal error in lexer: start code stack empty on getStartCode"
     code:_ -> return code 
{-
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

-}

-- ---------------------------------------------------------------------
-- From Language.Python.Common.LexerUtils

-- Functionality required by Alex 

type AlexInput = (SrcLocation, String)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar _ = error "alexInputPrevChar not used"

alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar (loc, input) 
   | null input  = Nothing
   | otherwise = Just (nextChar, (nextLoc, rest))
   where
   nextChar = head input
   rest = tail input 
   nextLoc = moveChar nextChar loc

moveChar :: Char -> SrcLocation -> SrcLocation 
moveChar '\n' = incLine 1 
moveChar '\t' = incTab 
moveChar '\r' = id 
moveChar _    = incColumn 1 

lexicalError :: P a
lexicalError = do
  location <- getLocation
  c <- liftM head getInput
  throwError $ UnexpectedChar c location

readOctNoO :: String -> Integer
readOctNoO (zero:rest) = read (zero:'O':rest)

-- ---------------------------------------------------------------------

type StartCode = Int
type Action = SrcSpan -> Int -> String -> P Token 

symbolToken :: (SrcSpan -> Token) -> Action 
symbolToken mkToken location _ _ = return (mkToken location)
