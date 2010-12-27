{
  
module Language.JavaScript.Parser.Lexer (
    Token(..)
    , lexCont 
    ) where
  
--import Control.Monad
import Language.JavaScript.Parser.LexerUtils
import Language.JavaScript.Parser.ParserMonad 
import Language.JavaScript.Parser.SrcLocation
import Language.JavaScript.Parser.Token
import qualified Data.Map as Map
--import Data.Word (Word8)

import Codec.Binary.UTF8.Light as UTF8

}

-- Not using a wrapper, rolling own below.
--%wrapper "basic"
--%wrapper "monad" 
--%wrapper "monadUserState" 
-- %wrapper "monad-bytestring" 

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

$any_char = [\x00-\xff]


$eol_char = [\x000A\x000D\x2028\x2029] -- any end of line character
--$eol_char = [$lf $cr] -- any end of line character
$not_eol_char = ~$eol_char -- anything but an end of line character


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
--$RegExpChars = [$alpha $digit \^\$\*\+\?\{\}\|\-\.\,\#\[\]\_\<\>]
--$RegExpChars = [$printable] # [\\]
-- {Non Terminator} = {String Chars1} - {CR} - {LF}
--$NonTerminator = $StringChars1 # [$cr $lf]
$NonTerminator = [$printable] # [$cr $lf]
-- {Non Zero Digits}={Digit}-[0]

-- ~ (LineTerminator | MUL | BSLASH | DIV)
$RegExpFirstChar = [$printable] # [ $cr $lf \* \\ \/]
-- ~ ( LineTerminator | BSLASH | DIV )
$RegExpChars = [$printable] # [ $cr $lf \\ \/]

$MultiLineNotAsteriskChar               = [$any_char] # [\*]
$MultiLineNotForwardSlashOrAsteriskChar = [$any_char] # [\* \/]

-- See http://blog.stevenlevithan.com/archives/javascript-regex-and-unicode
    -- *  \u0009 — Tab — \t
    -- * \u000a — Line feed — \n — (newline character)
    -- * \u000b — Vertical tab — \v
    -- * \u000c — Form feed — \f
    -- * \u000d — Carriage return — \r — (newline character)
    -- * \u0020 — Space
    -- * \u00a0 — No-break space
    -- * \u1680 — Ogham space mark
    -- * \u180e — Mongolian vowel separator
    -- * \u2000 — En quad
    -- * \u2001 — Em quad
    -- * \u2002 — En space
    -- * \u2003 — Em space
    -- * \u2004 — Three-per-em space
    -- * \u2005 — Four-per-em space
    -- * \u2006 — Six-per-em space
    -- * \u2007 — Figure space
    -- * \u2008 — Punctuation space
    -- * \u2009 — Thin space
    -- * \u200a — Hair space
    -- * \u2028 — Line separator — (newline character)
    -- * \u2029 — Paragraph separator — (newline character)
    -- * \u202f — Narrow no-break space
    -- * \u205f — Medium mathematical space
    -- * \u3000 — Ideographic space

--$white_char   = [\ \f\v\t\r\n]
$white_char = [\x0009\x000a\x000b\x000c\x000d\x0020\x00a0\x1680\x180e\x2000\x2001\x2002\x2003\x2004\x2005\x2006\x2007\x2008\x2009\x200a\x2028\x2029\x202f\x205f\x3000]


-- ! ------------------------------------------------- Terminals
tokens :-

-- State: 0 is regex allowed, 1 is / or /= allowed

<0> () ; -- { registerStates lexToken reg divide }

-- Skip Whitespace
<reg,divide> $white_char+   ;

-- Skip one line comment
<reg,divide> "//"($not_eol_char)*   ;


-- ---------------------------------------------------------------------
-- Comment definition from the ECMAScript spec, ver 3

-- MultiLineComment ::
--        /* MultiLineCommentChars(opt) */
-- MultiLineCommentChars ::
--        MultiLineNotAsteriskChar MultiLineCommentChars(opt)
--        * PostAsteriskCommentChars(opt)
-- PostAsteriskCommentChars ::
--        MultiLineNotForwardSlashOrAsteriskChar MultiLineCommentChars(opt)
--        * PostAsteriskCommentChars(opt)
-- MultiLineNotAsteriskChar ::
--        SourceCharacter but not asterisk *
-- MultiLineNotForwardSlashOrAsteriskChar ::
--        SourceCharacter but not forward-slash / or asterisk *

-- Skip multi-line comments. Note: may not nest
-- <reg,divide> "/*"($any_char)*"*/"  ;	
<reg,divide> "/*" (($MultiLineNotAsteriskChar)*| ("*")+ ($MultiLineNotForwardSlashOrAsteriskChar) )* ("*")+ "/"  ;	



-- Identifier    = {ID Head}{ID Tail}*
<reg,divide> @IDHead(@IDTail)*  { \loc len str -> keywordOrIdent (take len str) loc }

-- StringLiteral = '"' ( {String Chars1} | '\' {Printable} )* '"' 
--                | '' ( {String Chars2} | '\' {Printable} )* ''
<reg,divide>  $dq ( $StringChars1 | \\ $printable )* $dq
     | $sq ( $StringChars2 | \\ $printable )* $sq { mkString stringToken }

-- HexIntegerLiteral = '0x' {Hex Digit}+
<reg,divide> "0x" @HexDigit+ { mkString hexIntegerToken }

-- RegExp         = '/' ({RegExp Chars} | '\' {Non Terminator})+ '/' ( 'g' | 'i' | 'm' )*
-- <reg> "/" ($RegExpChars | "\" $NonTerminator)+ "/" ("g"|"i"|"m")* { mkString regExToken }

-- Based on the Jint version
<reg> "/" ($RegExpFirstChar | "\" $NonTerminator)  ($RegExpChars | "\" $NonTerminator)* "/" ("g"|"i"|"m")* { mkString regExToken }




-- DecimalLiteral= {Non Zero Digits}+ '.' {Digit}* ('e' | 'E' ) {Non Zero Digits}+ {Digit}* 
--              |  {Non Zero Digits}+ '.' {Digit}* 
--              | '0' '.' {Digit}+ ('e' | 'E' ) {Non Zero Digits}+ {Digit}* 
--              | {Non Zero Digits}+ {Digit}* 
--              | '0' 
--              | '0' '.' {Digit}+
<reg,divide> $non_zero_digit+ "." $digit* ("e"|"E") $non_zero_digit+ $digit* 
    | $non_zero_digit+ "." $digit*       
    | "0." $digit+  ("e"|"E") $non_zero_digit+ $digit* 
    | $non_zero_digit+ $digit*
    | "0"
    | "0." $digit+                    { mkString decimalToken }


-- beginning of file
<bof> {
   @eol_pattern                         ;
   -- @eol_pattern                         { endOfLine lexToken }
   --@eol_pattern                         { endOfLine alexMonadScan }
}

-- / or /= only allowed in state 1
<divide> {
     "/="       { mkString assignToken}
     "/"	{ symbolToken  DivToken}
    }

<reg,divide> {
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
     "*=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | ">>>=" | "&=" | "^=" | "|="
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





{

{-   
-- The next function select between the two lex input states, as called for in
-- secion 7 of ECMAScript Language Specification, Edition 3, 24 March 2000.   

The method is inspired by the lexer in http://jint.codeplex.com/

-}
classifyToken :: Token -> Int
classifyToken token = 
   case token of
      IdentifierToken {} -> divide
      NullToken {} -> divide
      TrueToken {} -> divide
      FalseToken {} -> divide
      ThisToken {} -> divide
      -- OctalToken {} -> divide -- May have to extend parser to cope with these
      DecimalToken {} -> divide
      HexIntegerToken {} -> divide
      StringToken {} -> divide
      RightCurlyToken {} -> divide
      RightParenToken {} -> divide
      _other      -> reg


-- Each right-hand side has type :: String -> Token

lexToken :: P Token
lexToken = do
  -- location <- getLocation
  -- input <- getInput
  input@(_,_,_,inp) <- alexGetInput
  -- startCode <- getStartCode
  lt <- getLastToken
  -- case alexScan (location, input) (classifyToken lt) of
  case alexScan input (classifyToken lt) of
    AlexEOF -> return endOfFileToken
    AlexError _ -> lexicalError
    AlexSkip rest _len -> do
       -- setLocation nextLocation 
       -- setInput rest 
       alexSetInput rest 
       lexToken
    AlexToken rest len action -> do
       --setLocation nextLocation 
       --setInput rest 
       alexSetInput rest
       --token <- action (mkSrcSpan location $ decColumn 1 nextLocation) len input 
       token <- action (ignorePendingBytes input) len inp
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
      --tok <- alexMonadScan
      case tok of
         {-
         CommentToken {} -> do
            addComment tok
            lexLoop
         LineJoinToken {} -> lexLoop
         -}
         _other -> cont tok


utf8Encode :: Char -> [Byte]
utf8Encode c = head (UTF8.encodeUTF8' [UTF8.c2w c])

--alexEOF = EOFToken alexSpanEmpty

ignorePendingBytes :: forall t t1 t2 t3. (t, t1, t2, t3) -> (t, t1, t3)
ignorePendingBytes (p,c,_ps,s) = (p,c,s)


alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,bs,s) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (_p,_c,[],[]) = Nothing
alexGetByte (p,_,[],(c:s))  = let p' = alexMove p c 
                                  (b:bs) = utf8Encode c
                              in p' `seq`  Just (b, (p', c, bs, s))

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l _c) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)

-- ---------------------------------------------------------------------
         
-- a keyword or an identifier (the syntax overlaps)
keywordOrIdent :: String -> AlexSpan -> P Token
keywordOrIdent str location
   = return $ case Map.lookup str keywords of
         Just symbol -> symbol location
         Nothing -> IdentifierToken location str  

-- mapping from strings to keywords
keywords :: Map.Map String (AlexSpan -> Token) 
keywords = Map.fromList keywordNames 

keywordNames :: [(String, AlexSpan -> Token)]
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
