-----------------------------------------------------------------------------
-- |
-- Module      : Language.Python.Common.LexerUtils 
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Various utilities to support the Python lexer. 
-----------------------------------------------------------------------------

module Language.JavaScript.Parser.LexerUtils where

import Control.Monad (liftM)
import Control.Monad.Error.Class (throwError)
import Data.List (foldl')
--import Data.Map as Map hiding (null, map)
import Numeric (readHex, readOct)
import Language.JavaScript.Parser.Token as Token 
import Language.JavaScript.Parser.ParserMonad hiding (location)
import Language.JavaScript.Parser.SrcLocation 

-- Beginning of. BOF = beginning of file, BOL = beginning of line
data BO = BOF | BOL

-- Functions for building tokens 

type StartCode = Int
type Action = SrcSpan -> Int -> String -> P Token 

lineJoin :: Action
lineJoin span _len _str = 
   return $ LineJoinToken $ spanStartPoint span

endOfLine :: P Token -> Action
endOfLine lexToken span _len _str = do
   setLastEOL $ spanStartPoint span
   lexToken

bolEndOfLine :: P Token -> Int -> Action 
bolEndOfLine lexToken bol span len inp = do
   pushStartCode bol 
   endOfLine lexToken span len inp

dedentation :: P Token -> Action
dedentation lexToken span _len _str = do
   topIndent <- getIndent
   -- case compare (endCol span) topIndent of
   case compare (startCol span) topIndent of
      EQ -> do popStartCode
               lexToken 
      LT -> do popIndent
               return dedentToken 
      GT -> spanError span "indentation error"

indentation :: P Token -> Int -> BO -> Action 
-- Check if we are at the EOF. If yes, we may need to generate a newline,
-- in case we came here from BOL (but not BOF).
indentation lexToken _dedentCode bo _loc _len [] = do
   popStartCode
   case bo of
      BOF -> lexToken
      BOL -> newlineToken
indentation lexToken dedentCode bo span _len _str = do
   popStartCode
   parenDepth <- getParenStackDepth
   if parenDepth > 0
      then lexToken
      else do 
         topIndent <- getIndent
         -- case compare (endCol span) topIndent of
         case compare (startCol span) topIndent of
            EQ -> case bo of
                     BOF -> lexToken
                     BOL -> newlineToken   
            LT -> do pushStartCode dedentCode 
                     newlineToken 
            -- GT -> do pushIndent (endCol span)
            GT -> do pushIndent (startCol span)
                     return indentToken
   where
   indentToken = IndentToken span

symbolToken :: (SrcSpan -> Token) -> Action 
symbolToken mkToken location _ _ = return (mkToken location)

token :: (SrcSpan -> String -> a -> Token) -> (String -> a) -> Action 
token mkToken read location len str 
   = return $ mkToken location literal (read literal) 
   where
   literal = take len str

-- special tokens for the end of file and end of line
endOfFileToken :: Token
endOfFileToken = EOFToken SpanEmpty
dedentToken = DedentToken SpanEmpty 

newlineToken :: P Token
newlineToken = do
   loc <- getLastEOL
   return $ NewlineToken loc

-- Test if we are at the end of the line or file
atEOLorEOF :: a -> AlexInput -> Int -> AlexInput -> Bool
atEOLorEOF _user _inputBeforeToken _tokenLength (_loc, inputAfterToken) 
   = null inputAfterToken || nextChar == '\n' || nextChar == '\r'
   where
   nextChar = head inputAfterToken 

notEOF :: a -> AlexInput -> Int -> AlexInput -> Bool
notEOF _user _inputBeforeToken _tokenLength (_loc, inputAfterToken) 
   = not (null inputAfterToken)

readBinary :: String -> Integer
readBinary 
   = toBinary . drop 2 
   where
   toBinary = foldl' acc 0
   acc b '0' = 2 * b
   acc b '1' = 2 * b + 1

readFloat :: String -> Double
readFloat str@('.':cs) = read ('0':readFloatRest str)
readFloat str = read (readFloatRest str)
readFloatRest :: String -> String
readFloatRest [] = []
readFloatRest ['.'] = ".0"
readFloatRest (c:cs) = c : readFloatRest cs

mkString :: (SrcSpan -> String -> Token) -> Action
mkString toToken loc len str = do
   return $ toToken loc (take len str)

stringToken :: SrcSpan -> String -> Token
stringToken loc str = StringToken loc str

rawStringToken :: SrcSpan -> String -> Token
rawStringToken loc str = StringToken loc str

byteStringToken :: SrcSpan -> String -> Token
byteStringToken loc str = ByteStringToken loc $ str

rawByteStringToken :: SrcSpan -> String -> Token
rawByteStringToken loc str = ByteStringToken loc $ str

openParen :: (SrcSpan -> Token) -> Action
openParen mkToken loc _len _str = do
   let token = mkToken loc
   pushParen token 
   return token 

closeParen :: (SrcSpan -> Token) -> Action
closeParen mkToken loc _len _str = do
  let token = mkToken loc
  topParen <- getParen
  case topParen of
     Nothing -> spanError loc err1 
     Just open -> if matchParen open token 
                    then popParen >> return token
                    else spanError loc err2
   where
   -- XXX fix these error messages
   err1 = "Lexical error ! unmatched closing paren"
   err2 = "Lexical error ! unmatched closing paren"

matchParen :: Token -> Token -> Bool
matchParen (LeftRoundBracketToken {}) (RightRoundBracketToken {}) = True
matchParen (LeftBraceToken {}) (RightBraceToken {}) = True
matchParen (LeftSquareBracketToken {}) (RightSquareBracketToken {}) = True
matchParen _ _ = False

-- -----------------------------------------------------------------------------
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
