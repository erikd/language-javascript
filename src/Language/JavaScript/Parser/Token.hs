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
import Language.JavaScript.Parser.SrcLocation (AlexSpan)
import Data.Data

-- | Lexical tokens.
data Token 
   -- Comment
   = CommentToken { token_span :: !AlexSpan, token_literal :: !String } -- ^ Single line comment.

   -- Identifiers 
   | IdentifierToken { token_span :: !AlexSpan, token_literal :: !String }            -- ^ Identifier.

   -- Javascript Literals  
     
   | DecimalToken { token_span :: !AlexSpan, token_literal :: !String  }
     -- ^ Literal: Decimal 
   | HexIntegerToken { token_span :: !AlexSpan, token_literal :: !String  }
     -- ^ Literal: Hexadecimal Integer
   | StringToken { token_span :: !AlexSpan, token_literal :: !String, token_delimiter :: !Char }                   
     -- ^ Literal: string, delimited by either single or double quotes
   | RegExToken { token_span :: !AlexSpan, token_literal :: !String  }
     -- ^ Literal: Regular Expression

   -- Keywords
   | BreakToken { token_span :: !AlexSpan, token_literal :: !String }
   | CaseToken { token_span :: !AlexSpan, token_literal :: !String }
   | CatchToken { token_span :: !AlexSpan, token_literal :: !String }
   | ConstToken { token_span :: !AlexSpan, token_literal :: !String }
   | ContinueToken { token_span :: !AlexSpan, token_literal :: !String }
   | DebuggerToken { token_span :: !AlexSpan, token_literal :: !String }
   | DefaultToken { token_span :: !AlexSpan, token_literal :: !String }
   | DeleteToken { token_span :: !AlexSpan, token_literal :: !String }
   | DoToken { token_span :: !AlexSpan, token_literal :: !String }
   | ElseToken { token_span :: !AlexSpan, token_literal :: !String }
   | EnumToken { token_span :: !AlexSpan, token_literal :: !String }
   | FalseToken { token_span :: !AlexSpan, token_literal :: !String }
   | FinallyToken { token_span :: !AlexSpan, token_literal :: !String }
   | ForToken { token_span :: !AlexSpan, token_literal :: !String }
   | FunctionToken { token_span :: !AlexSpan, token_literal :: !String }
   | IfToken { token_span :: !AlexSpan, token_literal :: !String }
   | InToken { token_span :: !AlexSpan, token_literal :: !String }
   | InstanceofToken { token_span :: !AlexSpan, token_literal :: !String }
   | NewToken { token_span :: !AlexSpan, token_literal :: !String }
   | NullToken { token_span :: !AlexSpan, token_literal :: !String }
   | ReturnToken { token_span :: !AlexSpan, token_literal :: !String }
   | SwitchToken { token_span :: !AlexSpan, token_literal :: !String }
   | ThisToken { token_span :: !AlexSpan, token_literal :: !String }
   | ThrowToken { token_span :: !AlexSpan, token_literal :: !String }
   | TrueToken { token_span :: !AlexSpan, token_literal :: !String }
   | TryToken { token_span :: !AlexSpan, token_literal :: !String }
   | TypeofToken { token_span :: !AlexSpan, token_literal :: !String }
   | VarToken { token_span :: !AlexSpan, token_literal :: !String }
   | VoidToken { token_span :: !AlexSpan, token_literal :: !String }
   | WhileToken { token_span :: !AlexSpan, token_literal :: !String }
   | WithToken { token_span :: !AlexSpan, token_literal :: !String }
   -- Future reserved words  
   | FutureToken { token_span :: !AlexSpan, token_literal :: !String }
   -- Needed, not sure what they are though.  
   | GetToken { token_span :: !AlexSpan, token_literal :: !String }
   | SetToken { token_span :: !AlexSpan, token_literal :: !String }
   
   -- Delimiters
   -- Operators
   | SemiColonToken { token_span :: !AlexSpan }
   | CommaToken { token_span :: !AlexSpan }
   | HookToken { token_span :: !AlexSpan }
   | ColonToken { token_span :: !AlexSpan }
   | OrToken { token_span :: !AlexSpan }
   | AndToken { token_span :: !AlexSpan }
   | BitwiseOrToken { token_span :: !AlexSpan }
   | BitwiseXorToken { token_span :: !AlexSpan }
   | BitwiseAndToken { token_span :: !AlexSpan }
   | StrictEqToken { token_span :: !AlexSpan }
   | EqToken { token_span :: !AlexSpan }
   | AssignToken { token_span :: !AlexSpan, token_literal :: !String }
   | SimpleAssignToken { token_span :: !AlexSpan }
   | StrictNeToken { token_span :: !AlexSpan }
   | NeToken { token_span :: !AlexSpan }
   | LshToken { token_span :: !AlexSpan }
   | LeToken { token_span :: !AlexSpan }
   | LtToken { token_span :: !AlexSpan }
   | UrshToken { token_span :: !AlexSpan }
   | RshToken { token_span :: !AlexSpan }
   | GeToken { token_span :: !AlexSpan }
   | GtToken { token_span :: !AlexSpan }
   | IncrementToken { token_span :: !AlexSpan }
   | DecrementToken { token_span :: !AlexSpan }
   | PlusToken { token_span :: !AlexSpan }
   | MinusToken { token_span :: !AlexSpan }
   | MulToken { token_span :: !AlexSpan }
   | DivToken { token_span :: !AlexSpan }
   | ModToken { token_span :: !AlexSpan }
   | NotToken { token_span :: !AlexSpan }
   | BitwiseNotToken { token_span :: !AlexSpan }
   | DotToken { token_span :: !AlexSpan }
   | LeftBracketToken { token_span :: !AlexSpan }
   | RightBracketToken { token_span :: !AlexSpan }
   | LeftCurlyToken { token_span :: !AlexSpan }
   | RightCurlyToken { token_span :: !AlexSpan }
   | LeftParenToken { token_span :: !AlexSpan }
   | RightParenToken { token_span :: !AlexSpan }
   | CondcommentEndToken { token_span :: !AlexSpan }

   -- Special cases
   | EOFToken { token_span :: !AlexSpan }                          -- ^ End of file 
   deriving (Eq,{-Ord,-}Show,Typeable{-,Data-})

{-
instance Span Token where
  getSpan = token_span 
-}   

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
