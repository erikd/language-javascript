{-# LANGUAGE CPP, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Python.Common.Token 
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Lexical tokens for the Python lexer. Contains the superset of tokens from
-- version 2 and version 3 of Python (they are mostly the same).
-----------------------------------------------------------------------------

module Language.JavaScript.Parser.Token ( 
   -- * The tokens 
   Token (..),
   -- * String conversion
   debugTokenString,
   tokenString,
   -- * Classification
   hasLiteral,
   TokenClass (..),
   classifyToken
   ) where

--import Language.JavaScript.Parser.Pretty
import Language.JavaScript.Parser.SrcLocation (SrcSpan (..), SrcLocation (..), Span(getSpan))
import Data.Data

-- | Lexical tokens.
data Token 
   -- Whitespace
   = IndentToken { token_span :: !SrcSpan }                       -- ^ Indentation: increase.
   | DedentToken { token_span :: !SrcSpan }                       -- ^ Indentation: decrease.
   | NewlineToken { token_span :: !SrcSpan }                      -- ^ Newline.
   | LineJoinToken { token_span :: !SrcSpan }                     -- ^ Line join (backslash at end of line).

   -- Comment
   | CommentToken { token_span :: !SrcSpan, token_literal :: !String } -- ^ Single line comment.

   -- Identifiers 
   | IdentifierToken { token_span :: !SrcSpan, token_literal :: !String }            -- ^ Identifier.

   -- Literals
   | StringToken { token_span :: !SrcSpan, token_literal :: !String }                   -- ^ Literal: string.
   | ByteStringToken { token_span :: !SrcSpan, token_literal :: !String }    -- ^ Literal: byte string.
   | IntegerToken { token_span :: !SrcSpan, token_literal :: !String, token_integer :: !Integer }                 
     -- ^ Literal: integer.
   | LongIntegerToken { token_span :: !SrcSpan, token_literal :: !String, token_integer :: !Integer }             
     -- ^ Literal: long integer. /Version 2 only/.
   | FloatToken { token_span :: !SrcSpan, token_literal :: !String, token_double :: !Double }                     
     -- ^ Literal: floating point.
   | ImaginaryToken { token_span :: !SrcSpan, token_literal :: !String, token_double :: !Double }                 
     -- ^ Literal: imaginary number.

   -- Keywords
   | DefToken { token_span :: !SrcSpan }                          -- ^ Keyword: \'def\'. 
   | WhileToken { token_span :: !SrcSpan }                        -- ^ Keyword: \'while\'.
   | IfToken { token_span :: !SrcSpan }                           -- ^ Keyword: \'if\'.
   | TrueToken { token_span :: !SrcSpan }                         -- ^ Keyword: \'True\'.
   | FalseToken { token_span :: !SrcSpan }                        -- ^ Keyword: \'False\'.
   | ReturnToken { token_span :: !SrcSpan }                       -- ^ Keyword: \'Return\'.
   | TryToken { token_span :: !SrcSpan }                          -- ^ Keyword: \'try\'.
   | ExceptToken { token_span :: !SrcSpan }                       -- ^ Keyword: \'except\'.
   | RaiseToken { token_span :: !SrcSpan }                        -- ^ Keyword: \'raise\'.
   | InToken { token_span :: !SrcSpan }                           -- ^ Keyword: \'in\'.
   | IsToken { token_span :: !SrcSpan }                           -- ^ Keyword: \'is\'.
   | LambdaToken { token_span :: !SrcSpan }                       -- ^ Keyword: \'lambda\'.
   | ClassToken { token_span :: !SrcSpan }                        -- ^ Keyword: \'class\'.
   | FinallyToken { token_span :: !SrcSpan }                      -- ^ Keyword: \'finally\'.
   | NoneToken { token_span :: !SrcSpan }                         -- ^ Keyword: \'None\'. 
   | ForToken { token_span :: !SrcSpan }                          -- ^ Keyword: \'for\'.
   | FromToken { token_span :: !SrcSpan }                         -- ^ Keyword: \'from\'.
   | GlobalToken { token_span :: !SrcSpan }                       -- ^ Keyword: \'global\'.
   | WithToken { token_span :: !SrcSpan }                         -- ^ Keyword: \'with\'.
   | AsToken { token_span :: !SrcSpan }                           -- ^ Keyword: \'as\'.
   | ElifToken { token_span :: !SrcSpan }                         -- ^ Keyword: \'elif\'.
   | YieldToken { token_span :: !SrcSpan }                        -- ^ Keyword: \'yield\'.
   | AssertToken { token_span :: !SrcSpan }                       -- ^ Keyword: \'assert\'.
   | ImportToken { token_span :: !SrcSpan }                       -- ^ Keyword: \'import\'.
   | PassToken { token_span :: !SrcSpan }                         -- ^ Keyword: \'pass\'.
   | BreakToken { token_span :: !SrcSpan }                        -- ^ Keyword: \'break\'.
   | ContinueToken { token_span :: !SrcSpan }                     -- ^ Keyword: \'continue\'.
   | DeleteToken { token_span :: !SrcSpan }                       -- ^ Keyword: \'del\'.
   | ElseToken { token_span :: !SrcSpan }                         -- ^ Keyword: \'else\'.
   | NotToken { token_span :: !SrcSpan }                          -- ^ Keyword: \'not\'.
   | AndToken { token_span :: !SrcSpan }                          -- ^ Keyword: boolean conjunction \'and\'.
   | OrToken { token_span :: !SrcSpan }                           -- ^ Keyword: boolean disjunction \'or\'.
   -- Version 3.x only:
   | NonLocalToken { token_span :: !SrcSpan }                     -- ^ Keyword: \'nonlocal\' (Python 3.x only)
   -- Version 2.x only:
   | PrintToken { token_span :: !SrcSpan }                        -- ^ Keyword: \'print\'. (Python 2.x only)
   | ExecToken { token_span :: !SrcSpan }                         -- ^ Keyword: \'exec\'. (Python 2.x only)

   -- Delimiters
   | AtToken { token_span :: !SrcSpan }                           -- ^ Delimiter: at sign \'\@\'. 
   | LeftRoundBracketToken { token_span :: !SrcSpan }             -- ^ Delimiter: left round bracket \'(\'.
   | RightRoundBracketToken { token_span :: !SrcSpan }            -- ^ Delimiter: right round bracket \')\'.
   | LeftSquareBracketToken { token_span :: !SrcSpan }            -- ^ Delimiter: left square bracket \'[\'.
   | RightSquareBracketToken { token_span :: !SrcSpan }           -- ^ Delimiter: right square bracket \']\'.
   | LeftBraceToken { token_span :: !SrcSpan }                    -- ^ Delimiter: left curly bracket \'{\'.
   | RightBraceToken { token_span :: !SrcSpan }                   -- ^ Delimiter: right curly bracket \'}\'.
   | DotToken { token_span :: !SrcSpan }                          -- ^ Delimiter: dot (full stop) \'.\'.
   | CommaToken { token_span :: !SrcSpan }                        -- ^ Delimiter: comma \',\'.
   | SemiColonToken { token_span :: !SrcSpan }                    -- ^ Delimiter: semicolon \';\'.
   | ColonToken { token_span :: !SrcSpan }                        -- ^ Delimiter: colon \':\'.
   | EllipsisToken { token_span :: !SrcSpan }                     -- ^ Delimiter: ellipses (three dots) \'...\'.
   | RightArrowToken { token_span :: !SrcSpan }                   -- ^ Delimiter: right facing arrow \'->\'.
   | AssignToken { token_span :: !SrcSpan }                       -- ^ Delimiter: assignment \'=\'.
   | PlusAssignToken { token_span :: !SrcSpan }                   -- ^ Delimiter: plus assignment \'+=\'.
   | MinusAssignToken { token_span :: !SrcSpan }                  -- ^ Delimiter: minus assignment \'-=\'.
   | MultAssignToken { token_span :: !SrcSpan }                   -- ^ Delimiter: multiply assignment \'*=\'
   | DivAssignToken { token_span :: !SrcSpan }                    -- ^ Delimiter: divide assignment \'/=\'.
   | ModAssignToken { token_span :: !SrcSpan }                    -- ^ Delimiter: modulus assignment \'%=\'.
   | PowAssignToken { token_span :: !SrcSpan }                    -- ^ Delimiter: power assignment \'**=\'.
   | BinAndAssignToken { token_span :: !SrcSpan }                 -- ^ Delimiter: binary-and assignment \'&=\'.
   | BinOrAssignToken { token_span :: !SrcSpan }                  -- ^ Delimiter: binary-or assignment \'|=\'.
   | BinXorAssignToken { token_span :: !SrcSpan }                 -- ^ Delimiter: binary-xor assignment \'^=\'.
   | LeftShiftAssignToken { token_span :: !SrcSpan }              -- ^ Delimiter: binary-left-shift assignment \'<<=\'.
   | RightShiftAssignToken { token_span :: !SrcSpan }             -- ^ Delimiter: binary-right-shift assignment \'>>=\'.
   | FloorDivAssignToken { token_span :: !SrcSpan }               -- ^ Delimiter: floor-divide assignment \'//=\'.
   | BackQuoteToken { token_span :: !SrcSpan }                    -- ^ Delimiter: back quote character \'`\'.

   -- Operators
   | PlusToken { token_span :: !SrcSpan }                         -- ^ Operator: plus \'+\'.
   | MinusToken { token_span :: !SrcSpan }                        -- ^ Operator: minus: \'-\'.
   | MultToken { token_span :: !SrcSpan }                         -- ^ Operator: multiply \'*\'.
   | DivToken { token_span :: !SrcSpan }                          -- ^ Operator: divide \'/\'.
   | GreaterThanToken { token_span :: !SrcSpan }                  -- ^ Operator: greater-than \'>\'.
   | LessThanToken { token_span :: !SrcSpan }                     -- ^ Operator: less-than \'<\'.
   | EqualityToken { token_span :: !SrcSpan }                     -- ^ Operator: equals \'==\'.
   | GreaterThanEqualsToken { token_span :: !SrcSpan }            -- ^ Operator: greater-than-or-equals \'>=\'.
   | LessThanEqualsToken { token_span :: !SrcSpan }               -- ^ Operator: less-than-or-equals \'<=\'.
   | ExponentToken { token_span :: !SrcSpan }                     -- ^ Operator: exponential \'**\'.
   | BitwiseOrToken { token_span :: !SrcSpan }                    -- ^ Operator: bitwise-or \'|\'.
   | XorToken { token_span :: !SrcSpan }                          -- ^ Operator: binary-xor \'^\'.
   | BinaryAndToken { token_span :: !SrcSpan }                    -- ^ Operator: binary-and \'&\'.
   | ShiftLeftToken { token_span :: !SrcSpan }                    -- ^ Operator: binary-shift-left \'<<\'.
   | ShiftRightToken { token_span :: !SrcSpan }                   -- ^ Operator: binary-shift-right \'>>\'.
   | ModuloToken { token_span :: !SrcSpan }                       -- ^ Operator: modulus \'%\'.
   | FloorDivToken { token_span :: !SrcSpan }                     -- ^ Operator: floor-divide \'//\'.
   | TildeToken { token_span :: !SrcSpan }                        -- ^ Operator: tilde \'~\'.
   | NotEqualsToken { token_span :: !SrcSpan }                    -- ^ Operator: not-equals \'!=\'.
   | NotEqualsV2Token { token_span :: !SrcSpan }                  -- ^ Operator: not-equals \'<>\'. Version 2 only.
   | HookToken { token_span :: !SrcSpan }                         -- ^ Operator: hook \'?\'.

   -- ++AZ++ to make the grammar work, for now
     | TokenLet { token_span :: !SrcSpan }
     | TokenIn { token_span :: !SrcSpan }
     | TokenInt { token_span :: !SrcSpan, token_literal :: !String, token_integer :: !Integer }                 
       -- ^ Literal: integer.
     | TokenVar { token_span :: !SrcSpan } --TODO: use real value
     | TokenEq { token_span :: !SrcSpan }
     | TokenPlus { token_span :: !SrcSpan }
     | TokenMinus { token_span :: !SrcSpan }
     | TokenTimes { token_span :: !SrcSpan }
     | TokenDiv { token_span :: !SrcSpan }
     | TokenOB { token_span :: !SrcSpan }
     | TokenCB { token_span :: !SrcSpan }
   -- ++AZ++ end



   -- Special cases
   | EOFToken { token_span :: !SrcSpan }                          -- ^ End of file 
   deriving (Eq,Ord,Show,Typeable,Data)

instance Span Token where
  getSpan = token_span 
   
-- | Produce a string from a token containing detailed information. Mainly intended for debugging. 
debugTokenString :: Token -> String
debugTokenString token = 
  "blah"
  {-
   render (text (show $ toConstr token) <+> pretty (token_span token) <+>
          if hasLiteral token then text (token_literal token) else empty)
   -}

-- | Test if a token contains its literal source text.
hasLiteral :: Token -> Bool
hasLiteral token =
   case token of
      CommentToken {}     -> True 
      IdentifierToken {}  -> True 
      StringToken {}      -> True 
      ByteStringToken {}  -> True 
      IntegerToken {}     -> True 
      LongIntegerToken {} -> True 
      FloatToken {}       -> True 
      ImaginaryToken  {}  -> True 
      other               -> False

-- | Classification of tokens
data TokenClass
   = Comment
   | Number
   | Identifier
   | Punctuation
   | Bracket
   | Layout 
   | Keyword
   | String
   | Operator
   | Assignment
   deriving (Show, Eq, Ord)

classifyToken :: Token -> TokenClass 
classifyToken token = 
   case token of
      IndentToken {} -> Layout 
      DedentToken {} -> Layout 
      NewlineToken {} -> Layout 
      CommentToken {} -> Comment 
      IdentifierToken {} -> Identifier 
      StringToken {} -> String 
      ByteStringToken {} -> String 
      IntegerToken {} -> Number 
      LongIntegerToken {} -> Number 
      FloatToken {} -> Number 
      ImaginaryToken {} -> Number 
      DefToken {} -> Keyword 
      WhileToken {} -> Keyword 
      IfToken {} ->  Keyword
      TrueToken {} -> Keyword 
      FalseToken {} -> Keyword 
      ReturnToken {} -> Keyword 
      TryToken {} -> Keyword 
      ExceptToken {} -> Keyword 
      RaiseToken {} -> Keyword 
      InToken {} -> Keyword 
      IsToken {} -> Keyword 
      LambdaToken {} -> Keyword 
      ClassToken {} -> Keyword 
      FinallyToken {} -> Keyword 
      NoneToken {} -> Keyword 
      ForToken {} -> Keyword 
      FromToken {} -> Keyword 
      GlobalToken {} -> Keyword 
      WithToken {} -> Keyword 
      AsToken {} -> Keyword 
      ElifToken {} -> Keyword 
      YieldToken {} -> Keyword 
      AssertToken {} -> Keyword 
      ImportToken {} -> Keyword 
      PassToken {} -> Keyword 
      BreakToken {} -> Keyword 
      ContinueToken {} -> Keyword 
      DeleteToken {} -> Keyword
      ElseToken {} -> Keyword 
      NotToken {} -> Keyword 
      AndToken {} -> Keyword 
      OrToken {} -> Keyword 
      NonLocalToken {} -> Keyword 
      PrintToken {} -> Keyword 
      ExecToken {} -> Keyword 
      AtToken {} -> Keyword 
      LeftRoundBracketToken {} -> Bracket 
      RightRoundBracketToken {} -> Bracket 
      LeftSquareBracketToken {} -> Bracket 
      RightSquareBracketToken {} -> Bracket 
      LeftBraceToken {} -> Bracket 
      RightBraceToken {} -> Bracket 
      DotToken {} -> Operator 
      CommaToken {} -> Punctuation 
      SemiColonToken {} -> Punctuation 
      ColonToken {} -> Punctuation 
      EllipsisToken {} -> Keyword  -- What kind of thing is an ellipsis?
      RightArrowToken {} -> Punctuation 
      AssignToken {} -> Assignment 
      PlusAssignToken {} -> Assignment 
      MinusAssignToken {} -> Assignment  
      MultAssignToken {} -> Assignment 
      DivAssignToken {} -> Assignment 
      ModAssignToken {} -> Assignment  
      PowAssignToken {} -> Assignment 
      BinAndAssignToken {} -> Assignment 
      BinOrAssignToken {} -> Assignment 
      BinXorAssignToken {} -> Assignment 
      LeftShiftAssignToken {} -> Assignment 
      RightShiftAssignToken {} -> Assignment 
      FloorDivAssignToken {} -> Assignment 
      BackQuoteToken {} -> Punctuation 
      PlusToken {} -> Operator 
      MinusToken {} -> Operator 
      MultToken {} -> Operator 
      DivToken {} -> Operator 
      GreaterThanToken {} -> Operator 
      LessThanToken {} -> Operator 
      EqualityToken {} -> Operator 
      GreaterThanEqualsToken {} -> Operator 
      LessThanEqualsToken {} -> Operator 
      ExponentToken {} -> Operator 
      -- BinaryOrToken {} -> Operator 
      XorToken {} -> Operator 
      BinaryAndToken {} -> Operator 
      ShiftLeftToken {} -> Operator 
      ShiftRightToken {} -> Operator 
      ModuloToken {} -> Operator 
      FloorDivToken {} -> Operator 
      TildeToken {} -> Operator 
      NotEqualsToken {} -> Operator 
      NotEqualsV2Token {} -> Operator 
      LineJoinToken {} -> Layout 
      EOFToken {} -> Layout  -- maybe a spurious classification.

-- | Produce a string from a token which is suitable for printing as Python concrete syntax.
-- /Invisible/ tokens yield an empty string.
tokenString :: Token -> String
tokenString token = 
   case token of
      IndentToken {} -> "" 
      DedentToken {} -> ""
      NewlineToken {} -> "" 
      CommentToken {} -> token_literal token
      IdentifierToken {} -> token_literal token
      StringToken {} -> token_literal token
      ByteStringToken {} -> token_literal token 
      IntegerToken {} -> token_literal token
      LongIntegerToken {} -> token_literal token
      FloatToken {} -> token_literal token
      ImaginaryToken {} -> token_literal token
      DefToken {} -> "def"
      WhileToken {} -> "while"
      IfToken {} -> "if"
      TrueToken {} -> "True"
      FalseToken {} -> "False"
      ReturnToken {} -> "return"
      TryToken {} -> "try"
      ExceptToken {} -> "except"
      RaiseToken {} -> "raise"
      InToken {} -> "in"
      IsToken {} -> "is"
      LambdaToken {} -> "lambda"
      ClassToken {} -> "class"
      FinallyToken {} -> "finally"
      NoneToken {} -> "None"
      ForToken {} -> "for"
      FromToken {} -> "from"
      GlobalToken {} -> "global"
      WithToken {} -> "with"
      AsToken {} -> "as"
      ElifToken {} -> "elif" 
      YieldToken {} -> "yield"
      AssertToken {} -> "assert" 
      ImportToken {} -> "import"
      PassToken {} -> "pass" 
      BreakToken {} -> "break" 
      ContinueToken {} -> "continue"
      DeleteToken {} -> "delete"
      ElseToken {} -> "else"
      NotToken {} -> "not"
      AndToken {} -> "and"
      OrToken {} -> "or"
      NonLocalToken {} -> "nonlocal"
      PrintToken {} -> "print"
      ExecToken {} -> "exec"
      AtToken {} -> "at"
      LeftRoundBracketToken {} -> "("
      RightRoundBracketToken {} -> ")"
      LeftSquareBracketToken {} -> "["
      RightSquareBracketToken {} -> "]"
      LeftBraceToken {} -> "{"
      RightBraceToken {} -> "}"
      DotToken {} -> "."
      CommaToken {} -> ","
      SemiColonToken {} -> ";"
      ColonToken {} -> ":"
      EllipsisToken {} -> "..."
      RightArrowToken {} -> "->"
      AssignToken {} -> "="
      PlusAssignToken {} -> "+="
      MinusAssignToken {} -> "-="
      MultAssignToken {} -> "*="
      DivAssignToken {} -> "/="
      ModAssignToken {} -> "%="
      PowAssignToken {} -> "**="
      BinAndAssignToken {} -> "&="
      BinOrAssignToken {} -> "|="
      BinXorAssignToken {} -> "^="
      LeftShiftAssignToken {} -> "<<="
      RightShiftAssignToken {} -> ">>="
      FloorDivAssignToken {} -> "//=" 
      BackQuoteToken {} -> "`"
      PlusToken {} -> "+"
      MinusToken {} -> "-"
      MultToken {} -> "*"
      DivToken {} -> "/"
      GreaterThanToken {} -> ">"
      LessThanToken {} -> "<"
      EqualityToken {} -> "=="
      GreaterThanEqualsToken {} -> ">="
      LessThanEqualsToken {} -> "<="
      ExponentToken {} -> "**"
      -- BinaryOrToken {} -> "|"
      XorToken {} -> "^"
      BinaryAndToken {} -> "&"
      ShiftLeftToken {} -> "<<"
      ShiftRightToken {} -> ">>"
      ModuloToken {} -> "%"
      FloorDivToken {} -> "//"
      TildeToken {} -> "~"
      NotEqualsToken {} -> "!="
      NotEqualsV2Token {} -> "<>"
      LineJoinToken {} -> "\\"
      EOFToken {} -> ""
