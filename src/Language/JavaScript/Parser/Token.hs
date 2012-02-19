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
   Token (..)
   -- * String conversion
   , debugTokenString
   -- * Classification
   -- TokenClass (..),
   ) where

import Data.Data
import Language.JavaScript.Parser.SrcLocation

-- | Lexical tokens.
data Token
   -- Comment
   = CommentToken { token_span :: !TokenPosn, token_literal :: !String } -- ^ Single line comment.

   -- Identifiers
   | IdentifierToken { token_span :: !TokenPosn, token_literal :: !String }            -- ^ Identifier.

   -- Javascript Literals

   | DecimalToken { token_span :: !TokenPosn, token_literal :: !String  }
     -- ^ Literal: Decimal
   | HexIntegerToken { token_span :: !TokenPosn, token_literal :: !String  }
     -- ^ Literal: Hexadecimal Integer
   | StringToken { token_span :: !TokenPosn, token_literal :: !String, token_delimiter :: !Char }
     -- ^ Literal: string, delimited by either single or double quotes
   | RegExToken { token_span :: !TokenPosn, token_literal :: !String  }
     -- ^ Literal: Regular Expression

   -- Keywords
   | BreakToken { token_span :: !TokenPosn, token_literal :: !String }
   | CaseToken { token_span :: !TokenPosn, token_literal :: !String }
   | CatchToken { token_span :: !TokenPosn, token_literal :: !String }
   | ConstToken { token_span :: !TokenPosn, token_literal :: !String }
   | ContinueToken { token_span :: !TokenPosn, token_literal :: !String }
   | DebuggerToken { token_span :: !TokenPosn, token_literal :: !String }
   | DefaultToken { token_span :: !TokenPosn, token_literal :: !String }
   | DeleteToken { token_span :: !TokenPosn, token_literal :: !String }
   | DoToken { token_span :: !TokenPosn, token_literal :: !String }
   | ElseToken { token_span :: !TokenPosn, token_literal :: !String }
   | EnumToken { token_span :: !TokenPosn, token_literal :: !String }
   | FalseToken { token_span :: !TokenPosn, token_literal :: !String }
   | FinallyToken { token_span :: !TokenPosn, token_literal :: !String }
   | ForToken { token_span :: !TokenPosn, token_literal :: !String }
   | FunctionToken { token_span :: !TokenPosn, token_literal :: !String }
   | IfToken { token_span :: !TokenPosn, token_literal :: !String }
   | InToken { token_span :: !TokenPosn, token_literal :: !String }
   | InstanceofToken { token_span :: !TokenPosn, token_literal :: !String }
   | NewToken { token_span :: !TokenPosn, token_literal :: !String }
   | NullToken { token_span :: !TokenPosn, token_literal :: !String }
   | ReturnToken { token_span :: !TokenPosn, token_literal :: !String }
   | SwitchToken { token_span :: !TokenPosn, token_literal :: !String }
   | ThisToken { token_span :: !TokenPosn, token_literal :: !String }
   | ThrowToken { token_span :: !TokenPosn, token_literal :: !String }
   | TrueToken { token_span :: !TokenPosn, token_literal :: !String }
   | TryToken { token_span :: !TokenPosn, token_literal :: !String }
   | TypeofToken { token_span :: !TokenPosn, token_literal :: !String }
   | VarToken { token_span :: !TokenPosn, token_literal :: !String }
   | VoidToken { token_span :: !TokenPosn, token_literal :: !String }
   | WhileToken { token_span :: !TokenPosn, token_literal :: !String }
   | WithToken { token_span :: !TokenPosn, token_literal :: !String }
   -- Future reserved words
   | FutureToken { token_span :: !TokenPosn, token_literal :: !String }
   -- Needed, not sure what they are though.
   | GetToken { token_span :: !TokenPosn, token_literal :: !String }
   | SetToken { token_span :: !TokenPosn, token_literal :: !String }

   -- Delimiters
   -- Operators
   | SemiColonToken { token_span :: !TokenPosn }
   | CommaToken { token_span :: !TokenPosn }
   | HookToken { token_span :: !TokenPosn }
   | ColonToken { token_span :: !TokenPosn }
   | OrToken { token_span :: !TokenPosn }
   | AndToken { token_span :: !TokenPosn }
   | BitwiseOrToken { token_span :: !TokenPosn }
   | BitwiseXorToken { token_span :: !TokenPosn }
   | BitwiseAndToken { token_span :: !TokenPosn }
   | StrictEqToken { token_span :: !TokenPosn }
   | EqToken { token_span :: !TokenPosn }
   | AssignToken { token_span :: !TokenPosn, token_literal :: !String }
   | SimpleAssignToken { token_span :: !TokenPosn }
   | StrictNeToken { token_span :: !TokenPosn }
   | NeToken { token_span :: !TokenPosn }
   | LshToken { token_span :: !TokenPosn }
   | LeToken { token_span :: !TokenPosn }
   | LtToken { token_span :: !TokenPosn }
   | UrshToken { token_span :: !TokenPosn }
   | RshToken { token_span :: !TokenPosn }
   | GeToken { token_span :: !TokenPosn }
   | GtToken { token_span :: !TokenPosn }
   | IncrementToken { token_span :: !TokenPosn }
   | DecrementToken { token_span :: !TokenPosn }
   | PlusToken { token_span :: !TokenPosn }
   | MinusToken { token_span :: !TokenPosn }
   | MulToken { token_span :: !TokenPosn }
   | DivToken { token_span :: !TokenPosn }
   | ModToken { token_span :: !TokenPosn }
   | NotToken { token_span :: !TokenPosn }
   | BitwiseNotToken { token_span :: !TokenPosn }
   | DotToken { token_span :: !TokenPosn }
   | LeftBracketToken { token_span :: !TokenPosn }
   | RightBracketToken { token_span :: !TokenPosn }
   | LeftCurlyToken { token_span :: !TokenPosn }
   | RightCurlyToken { token_span :: !TokenPosn }
   | LeftParenToken { token_span :: !TokenPosn }
   | RightParenToken { token_span :: !TokenPosn }
   | CondcommentEndToken { token_span :: !TokenPosn }

   -- Special cases
   | EOFToken { token_span :: !TokenPosn }                          -- ^ End of file
   deriving (Eq,{-Ord,-}Show,Typeable{-,Data-})


-- | Produce a string from a token containing detailed information. Mainly intended for debugging.
debugTokenString :: Token -> String
debugTokenString _token =
  "blah"
  {-
   render (text (show $ toConstr token) <+> pretty (token_span token) <+>
          if hasLiteral token then text (token_literal token) else empty)
   -}


-- EOF
