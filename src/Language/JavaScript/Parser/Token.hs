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
   -- tokenString,
   -- * Classification
   hasLiteral,
   TokenClass (..),
   -- classifyToken
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
     
   -- Javascript Literals  
     
   | DecimalToken { token_span :: !SrcSpan, token_literal :: !String  }
     -- ^ Literal: Decimal 
   | HexIntegerToken { token_span :: !SrcSpan, token_literal :: !String  }
     -- ^ Literal: Hexadecimal Integer
     

   -- Keywords
   | BreakToken { token_span :: !SrcSpan }
   | CaseToken { token_span :: !SrcSpan }
   | CatchToken { token_span :: !SrcSpan }
   | ConstToken { token_span :: !SrcSpan }
   | ContinueToken { token_span :: !SrcSpan }
   | DebuggerToken { token_span :: !SrcSpan }
   | DefaultToken { token_span :: !SrcSpan }
   | DeleteToken { token_span :: !SrcSpan }
   | DoToken { token_span :: !SrcSpan }
   | ElseToken { token_span :: !SrcSpan }
   | EnumToken { token_span :: !SrcSpan }
   | FalseToken { token_span :: !SrcSpan }
   | FinallyToken { token_span :: !SrcSpan }
   | ForToken { token_span :: !SrcSpan }
   | FunctionToken { token_span :: !SrcSpan }
   | IfToken { token_span :: !SrcSpan }
   | InToken { token_span :: !SrcSpan }
   | InstanceofToken { token_span :: !SrcSpan }
   | NewToken { token_span :: !SrcSpan }
   | NullToken { token_span :: !SrcSpan }
   | ReturnToken { token_span :: !SrcSpan }
   | SwitchToken { token_span :: !SrcSpan }
   | ThisToken { token_span :: !SrcSpan }
   | ThrowToken { token_span :: !SrcSpan }
   | TrueToken { token_span :: !SrcSpan }
   | TryToken { token_span :: !SrcSpan }
   | TypeofToken { token_span :: !SrcSpan }
   | VarToken { token_span :: !SrcSpan }
   | VoidToken { token_span :: !SrcSpan }
   | WhileToken { token_span :: !SrcSpan }
   | WithToken { token_span :: !SrcSpan }

   -- Delimiters
   -- Operators
   | SemiColonToken { token_span :: !SrcSpan }
   | CommaToken { token_span :: !SrcSpan }
   | HookToken { token_span :: !SrcSpan }
   | ColonToken { token_span :: !SrcSpan }
   | OrToken { token_span :: !SrcSpan }
   | AndToken { token_span :: !SrcSpan }
   | BitwiseOrToken { token_span :: !SrcSpan }
   | BitwiseXorToken { token_span :: !SrcSpan }
   | BitwiseAndToken { token_span :: !SrcSpan }
   | StrictEqToken { token_span :: !SrcSpan }
   | EqToken { token_span :: !SrcSpan }
   | AssignToken { token_span :: !SrcSpan }
   | StrictNeToken { token_span :: !SrcSpan }
   | NeToken { token_span :: !SrcSpan }
   | LshToken { token_span :: !SrcSpan }
   | LeToken { token_span :: !SrcSpan }
   | LtToken { token_span :: !SrcSpan }
   | UrshToken { token_span :: !SrcSpan }
   | RshToken { token_span :: !SrcSpan }
   | GeToken { token_span :: !SrcSpan }
   | GtToken { token_span :: !SrcSpan }
   | IncrementToken { token_span :: !SrcSpan }
   | DecrementToken { token_span :: !SrcSpan }
   | PlusToken { token_span :: !SrcSpan }
   | MinusToken { token_span :: !SrcSpan }
   | MulToken { token_span :: !SrcSpan }
   | DivToken { token_span :: !SrcSpan }
   | ModToken { token_span :: !SrcSpan }
   | NotToken { token_span :: !SrcSpan }
   | BitwiseNotToken { token_span :: !SrcSpan }
   | DotToken { token_span :: !SrcSpan }
   | LeftBracketToken { token_span :: !SrcSpan }
   | RightBracketToken { token_span :: !SrcSpan }
   | LeftCurlyToken { token_span :: !SrcSpan }
   | RightCurlyToken { token_span :: !SrcSpan }
   | LeftParenToken { token_span :: !SrcSpan }
   | RightParenToken { token_span :: !SrcSpan }
   | CondcommentEndToken { token_span :: !SrcSpan }

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

{-
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
-}

-- | Produce a string from a token which is suitable for printing as Python concrete syntax.
-- /Invisible/ tokens yield an empty string.
{-
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
-}

-- EOF
