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
$ht = \t  -- horizontal tab
$sq = '   -- single quote
$dq = \"  -- double quote
$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$digit    = 0-9
$non_zero_digit = 1-9
$ident_letter = [a-zA-Z_]
@eol_pattern = $lf | $cr $lf | $cr $lf  

-- From GOLD Parser
-- {ID Head}      = {Letter} + [_] + [$]
@IDHead = $alpha | [_] | [\$]

-- {ID Tail}      = {Alphanumeric} + [_] + [$]
@IDTail = $alpha | $digit | [_] | [\$]

-- {String Chars1} = {Printable} + {HT} - ["\] 
-- {String Chars2} = {Printable} + {HT} - [\''] 
$StringChars1 = [$printable $ht] # [$dq \\] 
$StringChars2 = [$printable $ht] # [$sq \\] 
$short_str_char = [^ \n \r ' \" \\]

-- {Hex Digit}    = {Digit} + [ABCDEF] + [abcdef]
@HexDigit = $digit | [a-fA-F]
-- {RegExp Chars} = {Letter}+{Digit}+['^']+['$']+['*']+['+']+['?']+['{']+['}']+['|']+['-']+['.']+[',']+['#']+['[']+[']']+['_']+['<']+['>']
$RegExpChars = [$alpha $digit \$\*\+\?\{\}\|\-\.\,\#\[\]\_\<\>]
-- {Non Terminator} = {String Chars1} - {CR} - {LF}
$NonTerminator = $StringChars1 # [$cr $lf]
-- {Non Zero Digits}={Digit}-[0]


-- WhiteSpace ::
--      <TAB>
--      <VT>
--      <FF>
--      <SP>
--      <NBSP>
--      <USP>
-- TODO: bring in NBSP and USP
$white_char   = [\ \f\v\t]

-- ! ------------------------------------------------- Terminals
tokens :-

-- Skip Whitespace
<0> $white_char+   ;

-- Identifier    = {ID Head}{ID Tail}*
<0> @IDHead(@IDTail)*  { \loc len str -> keywordOrIdent (take len str) loc }

-- StringLiteral = '"' ( {String Chars1} | '\' {Printable} )* '"' 
--                | '' ( {String Chars2} | '\' {Printable} )* ''
<0>  $dq ( $StringChars1 | \\ $printable )* $dq
   | $sq ( $StringChars2 | \\ $printable )* $sq { mkString stringToken }

-- HexIntegerLiteral = '0x' {Hex Digit}+
<0> "0x" @HexDigit+ { mkString hexIntegerToken }

-- RegExp         = '/' ({RegExp Chars} | '\' {Non Terminator})+ '/' ( 'g' | 'i' | 'm' )*
<0> "/" ($RegExpChars | "\\" $NonTerminator)+ "/" ("g"|"i"|"m")* { mkString regExToken }

-- DecimalLiteral= {Non Zero Digits}+ '.' {Digit}* ('e' | 'E' ) {Non Zero Digits}+ {Digit}* 
--              |  {Non Zero Digits}+ '.' {Digit}* 
--              | '0' '.' {Digit}+ ('e' | 'E' ) {Non Zero Digits}+ {Digit}* 
--              | {Non Zero Digits}+ {Digit}* 
--              | '0' 
--              | '0' '.' {Digit}+
<0> $non_zero_digit+ "." $digit* ("e"|"E") $non_zero_digit+ $digit* 
    | $non_zero_digit+ "." $digit*       
    | "0." $digit+  ("e"|"E") $non_zero_digit+ $digit* 
    | $non_zero_digit+ $digit*
    | "0"
    | "0." $digit+                    { mkString decimalToken }

-- Comment Start = '/*'
-- Comment End   = '*/'
-- Comment Line  = '//'


-- <0> {
--    @eol_pattern     { bolEndOfLine lexToken bol }  
-- }

-- beginning of file
<bof> {
   -- @eol_pattern                         ;
   @eol_pattern                         { endOfLine lexToken }
   -- ()                                   { indentation lexToken dedent BOF }
}


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
     "*=" | "/=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | ">>>=" | "&=" | "^=" | "|="
      	        { mkString assignToken}
     "="        { symbolToken  SimpleAssignToken}
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



-- Set emacs mode
-- Local Variables: 
-- mode:haskell
-- End:             
