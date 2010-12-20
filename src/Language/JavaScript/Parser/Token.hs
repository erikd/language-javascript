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
   -- TokenClass (..),
   ) where

--import Language.JavaScript.Parser.Pretty
import Language.JavaScript.Parser.SrcLocation (SrcSpan (..), Span(getSpan))
import Data.Data

-- | Lexical tokens.
data Token 
   -- Comment
   = CommentToken { token_span :: !SrcSpan, token_literal :: !String } -- ^ Single line comment.

   -- Identifiers 
   | IdentifierToken { token_span :: !SrcSpan, token_literal :: !String }            -- ^ Identifier.

   -- Javascript Literals  
     
   | DecimalToken { token_span :: !SrcSpan, token_literal :: !String  }
     -- ^ Literal: Decimal 
   | HexIntegerToken { token_span :: !SrcSpan, token_literal :: !String  }
     -- ^ Literal: Hexadecimal Integer
   | StringToken { token_span :: !SrcSpan, token_literal :: !String, token_delimiter :: !Char }                   
     -- ^ Literal: string, delimited by either single or double quotes
   | RegExToken { token_span :: !SrcSpan, token_literal :: !String  }
     -- ^ Literal: Regular Expression

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
   | AssignToken { token_span :: !SrcSpan, token_literal :: !String }
   | SimpleAssignToken { token_span :: !SrcSpan }
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

{-
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
-}

-- EOF
