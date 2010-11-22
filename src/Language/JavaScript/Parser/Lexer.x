{
  
module Language.JavaScript.Parser.Lexer (Token(..),lexCont {-, alexScanTokens-}) where
  
import Language.JavaScript.Parser.ParserMonad
import Language.JavaScript.Parser.SrcLocation
}

--%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

<0> {
     "let" { symbolToken TokenLet }
     "in"  { symbolToken TokenIn }
     "var" { symbolToken TokenVar } --TODO: use real value
     "="   {symbolToken TokenEq }
     "+"   {symbolToken TokenPlus }
     "-"   {symbolToken TokenMinus }
     "*"   {symbolToken TokenTimes }
     "/"   {symbolToken TokenDiv }
     "("   {symbolToken TokenOB }
     ")"  {symbolToken TokenCB }
}


{
-- Each right-hand side has type :: String -> Token

lexToken :: P Token
lexToken = do
  location <- getLocation
  input <- getInput
  startCode <- getStartCode
  case alexScan (location, input) startCode of
    AlexEOF -> return endOfFileToken
    AlexError _ -> lexicalError
    AlexSkip (nextLocation, rest) len -> do
       setLocation nextLocation 
       setInput rest 
       lexToken
    AlexToken (nextLocation, rest) len action -> do
       setLocation nextLocation 
       setInput rest 
       token <- action (mkSrcSpan location $ decColumn 1 nextLocation) len input 
       setLastToken token
       return token

-- This is called by the Happy parser.
lexCont :: (Token -> P a) -> P a
lexCont cont = do
   lexLoop
   where
   -- lexLoop :: P a
   lexLoop = do
      tok <- lexToken
      case tok of
         {-
         CommentToken {} -> do
            addComment tok
            lexLoop
         LineJoinToken {} -> lexLoop
         -}
         _other -> cont tok

-- lexer :: String -> [Token]
-- lexer [] = []
-- lexer (c:cs) 
--       | isSpace c = lexer cs
--       | isAlpha c = lexVar (c:cs)
--       | isDigit c = lexNum (c:cs)
-- lexer ('=':cs) = TokenEq : lexer cs
-- lexer ('+':cs) = TokenPlus : lexer cs
-- lexer ('-':cs) = TokenMinus : lexer cs
-- lexer ('*':cs) = TokenTimes : lexer cs
-- lexer ('/':cs) = TokenDiv : lexer cs
-- lexer ('(':cs) = TokenOB : lexer cs
-- lexer (')':cs) = TokenCB : lexer cs

-- lexNum cs = TokenInt (read num) : lexer rest
--       where (num,rest) = span isDigit cs

-- lexVar cs =
--    case span isAlpha cs of
--       ("let",rest) -> TokenLet : lexer rest
--       ("in",rest)  -> TokenIn : lexer rest
--       (var,rest)   -> TokenVar var : lexer rest

}



