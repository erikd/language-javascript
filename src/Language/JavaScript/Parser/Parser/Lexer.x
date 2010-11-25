{
  
module Language.JavaScript.Parser.Parser.Lexer (Token(..),lexCont {-, alexScanTokens-},initStartCodeStack) where
  
import Language.JavaScript.Parser.LexerUtils
import Language.JavaScript.Parser.ParserMonad
import Language.JavaScript.Parser.SrcLocation
import Language.JavaScript.Parser.Token
import qualified Data.Map as Map

}

--%wrapper "basic"

-- character sets
$lf = \n  -- line feed
$cr = \r  -- carriage return
$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$digit    = 0-9
$non_zero_digit = 1-9
$ident_letter = [a-zA-Z_]
@eol_pattern = $lf | $cr $lf | $cr $lf  



-- @reservedid = 
--          break|case|catch|const|continue|
--          debugger|default|delete|do|
--          else|enum|
--          false|finally|for|function|
--          if|in|instanceof|
--          new|null|
--          return|
--          switch|
--          this|throw|true|try|typeof|
--          var|void|
--          while|with

tokens :-

-- <0> {
--    @eol_pattern     { bolEndOfLine lexToken bol }  
-- }

-- beginning of file
<bof> {
   -- @eol_pattern                         ;
   @eol_pattern                         { endOfLine lexToken }
   -- ()                                   { indentation lexToken dedent BOF }
}

-- <0> $ident_letter($ident_letter|$digit)*  { \loc len str -> keywordOrIdent (take len str) loc }


<0> {
      \;	{ symbolToken  SemiColonToken}
      ","	{ symbolToken  CommaToken}

--      "?"	{ symbolToken  HookToken}
--      ":"	{ symbolToken  ColonToken}
--      "||"	{ symbolToken  OrToken}
--      "&&"	{ symbolToken  AndToken}
--      "|"	{ symbolToken  BitwiseOrToken}

--      -- "^"	{ symbolToken  "BITWISE_XOR"}
--      -- "&"	{ symbolToken  "BITWISE_AND"}
--      -- "==="	{ symbolToken  "STRICT_EQ"}
--      -- "=="	{ symbolToken  "EQ"}
--      -- "="	{ symbolToken  "ASSIGN"}
--      -- "!=="	{ symbolToken  "STRICT_NE"}
--      -- "!="	{ symbolToken  "NE"}
--      -- "<<"	{ symbolToken  "LSH"}
--      -- "<="	{ symbolToken  "LE"}
--      -- "<"	{ symbolToken  "LT"}
--      -- ">>>"	{ symbolToken  "URSH"}
--      -- ">>"	{ symbolToken  "RSH"}
--      -- ">="	{ symbolToken  "GE"}
--      -- ">"	{ symbolToken  "GT"}
--      -- "++"	{ symbolToken  "INCREMENT"}
--      -- "--"	{ symbolToken  "DECREMENT"}
--      -- "+"	{ symbolToken  "PLUS"}
--      -- "-"	{ symbolToken  "MINUS"}
--      -- "*"	{ symbolToken  "MUL"}
--      -- "/"	{ symbolToken  "DIV"}
--      -- "%"	{ symbolToken  "MOD"}
--      -- "!"	{ symbolToken  "NOT"}
--      -- "~"	{ symbolToken  "BITWISE_NOT"}
--      -- "."	{ symbolToken  "DOT"}
--      -- "["	{ symbolToken  "LEFT_BRACKET"}
--      -- "]"	{ symbolToken  "RIGHT_BRACKET"}
--      -- "{"	{ symbolToken  "LEFT_CURLY"}
--      -- "}"	{ symbolToken  "RIGHT_CURLY"}
--      -- "("	{ symbolToken  "LEFT_PAREN"}
--      -- ")"	{ symbolToken  "RIGHT_PAREN"}
--      -- "@*/"	{ symbolToken  "CONDCOMMENT_END"
}



-- <0> {
--      "let" { symbolToken TokenLet }

--      "in"  { symbolToken TokenIn }
--      -- "9"  { symbolToken TokenInt } --TODO: use real value\
--      $non_zero_digit $digit* { token TokenInt read }  
--      "var" { symbolToken TokenVar } --TODO: use real value
--      "="   {symbolToken TokenEq }
--      "+"   {symbolToken TokenPlus }
--      "-"   {symbolToken TokenMinus }
--      "*"   {symbolToken TokenTimes }
--      "/"   {symbolToken TokenDiv }
--      "("   {symbolToken TokenOB }
--      ")"   {symbolToken TokenCB }
-- }


{

-- The lexer starts off in the beginning of file state (bof)
initStartCodeStack :: [Int]
--initStartCodeStack = [bof,0]
initStartCodeStack = [0]

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
         
-- ---------------------------------------------------------------------         
         
-- a keyword or an identifier (the syntax overlaps)
keywordOrIdent :: String -> SrcSpan -> P Token
keywordOrIdent str location
   = return $ case Map.lookup str keywords of
         Just symbol -> symbol location
         Nothing -> IdentifierToken location str  

-- mapping from strings to keywords
keywords :: Map.Map String (SrcSpan -> Token) 
keywords = Map.fromList keywordNames 

keywordNames :: [(String, SrcSpan -> Token)]
keywordNames =
   [ ("False", FalseToken), ("class", ClassToken), ("finally", FinallyToken), ("is", IsToken), ("return", ReturnToken)
   , ("None", NoneToken), ("continue", ContinueToken), ("for", ForToken), ("lambda", LambdaToken), ("try", TryToken)
   , ("True", TrueToken), ("def", DefToken), ("from", FromToken), ("nonlocal", NonLocalToken), ("while", WhileToken)
   , ("and", AndToken), ("del", DeleteToken), ("global", GlobalToken), ("not", NotToken), ("with", WithToken)
   , ("as", AsToken), ("elif", ElifToken), ("if", IfToken), ("or", OrToken), ("yield", YieldToken)
   , ("assert", AssertToken), ("else", ElseToken), ("import", ImportToken), ("pass", PassToken)
   , ("break", BreakToken), ("except", ExceptToken), ("in", InToken), ("raise", RaiseToken)
   ]

}



