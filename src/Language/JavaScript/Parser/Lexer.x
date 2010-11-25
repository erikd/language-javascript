{
  
module Language.JavaScript.Parser.Lexer (Token(..),lexCont {-, alexScanTokens-},initStartCodeStack) where
  
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

<0> $ident_letter($ident_letter|$digit)*  { \loc len str -> keywordOrIdent (take len str) loc }


<0> {
      \;	{ symbolToken  SemiColonToken}
      ","	{ symbolToken  CommaToken}
     "?"	{ symbolToken  HookToken}
     ":"	{ symbolToken  ColonToken}
     "||"	{ symbolToken  OrToken}
     "&&"	{ symbolToken  AndToken}
     "|"	{ symbolToken  BitwiseOrToken}
     "^"	{ symbolToken  BitwiseXorToken}
     "&"	{ symbolToken  BitwiseAndToken}
     "==="	{ symbolToken  StrictEqToken}
     "=="	{ symbolToken  EqToken}
     "="	{ symbolToken  AssignToken}
     "!=="	{ symbolToken  StrictNeToken}
     "!="	{ symbolToken  NeToken}
     "<<"	{ symbolToken  LshToken}
     "<="	{ symbolToken  LeToken}
     "<"	{ symbolToken  LtToken}
     ">>>"	{ symbolToken  UrshToken}
     ">>"	{ symbolToken  RshToken}
     ">="	{ symbolToken  GeToken}
     ">"	{ symbolToken  GtToken}
     "++"	{ symbolToken  IncrementToken}
     "--"	{ symbolToken  DecrementToken}
     "+"	{ symbolToken  PlusToken}
     "-"	{ symbolToken  MinusToken}
     "*"	{ symbolToken  MulToken}
     "/"	{ symbolToken  DivToken}
     "%"	{ symbolToken  ModToken}
     "!"	{ symbolToken  NotToken}
     "~"	{ symbolToken  BitwiseNotToken}
     "."	{ symbolToken  DotToken}
     "["	{ symbolToken  LeftBracketToken}
     "]"	{ symbolToken  RightBracketToken}
     "{"	{ symbolToken  LeftCurlyToken}
     "}"	{ symbolToken  RightCurlyToken}
     "("	{ symbolToken  LeftParenToken}
     ")"	{ symbolToken  RightParenToken}
     "@*/"	{ symbolToken  CondcommentEndToken}
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
   [ 
    ("break",BreakToken),("case",CaseToken),("catch",CatchToken),("const",ConstToken),
    ("continue",ContinueToken),("debugger",DebuggerToken),("default",DefaultToken),
    ("delete",DeleteToken),("do",DoToken),("else",ElseToken),("enum",EnumToken),
    ("false",FalseToken),("finally",FinallyToken),("for",ForToken),
    ("function",FunctionToken),("if",IfToken),("in",InToken),
    ("instanceof",InstanceofToken),("new",NewToken),("null",NullToken),
    ("return",ReturnToken),("switch",SwitchToken),("this",ThisToken),
    ("throw",ThrowToken),("true",TrueToken),("try",TryToken),
    ("typeof",TypeofToken),("var",VarToken),("void",VoidToken),
    ("while",WhileToken),("with",WithToken)
   ]

}



