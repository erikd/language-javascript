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
   | BreakToken { token_span :: !AlexSpan }
   | CaseToken { token_span :: !AlexSpan }
   | CatchToken { token_span :: !AlexSpan }
   | ConstToken { token_span :: !AlexSpan }
   | ContinueToken { token_span :: !AlexSpan }
   | DebuggerToken { token_span :: !AlexSpan }
   | DefaultToken { token_span :: !AlexSpan }
   | DeleteToken { token_span :: !AlexSpan }
   | DoToken { token_span :: !AlexSpan }
   | ElseToken { token_span :: !AlexSpan }
   | EnumToken { token_span :: !AlexSpan }
   | FalseToken { token_span :: !AlexSpan }
   | FinallyToken { token_span :: !AlexSpan }
   | ForToken { token_span :: !AlexSpan }
   | FunctionToken { token_span :: !AlexSpan }
   | IfToken { token_span :: !AlexSpan }
   | InToken { token_span :: !AlexSpan }
   | InstanceofToken { token_span :: !AlexSpan }
   | NewToken { token_span :: !AlexSpan }
   | NullToken { token_span :: !AlexSpan }
   | ReturnToken { token_span :: !AlexSpan }
   | SwitchToken { token_span :: !AlexSpan }
   | ThisToken { token_span :: !AlexSpan }
   | ThrowToken { token_span :: !AlexSpan }
   | TrueToken { token_span :: !AlexSpan }
   | TryToken { token_span :: !AlexSpan }
   | TypeofToken { token_span :: !AlexSpan }
   | VarToken { token_span :: !AlexSpan }
   | VoidToken { token_span :: !AlexSpan }
   | WhileToken { token_span :: !AlexSpan }
   | WithToken { token_span :: !AlexSpan }
   -- Future reserved words  
   | FutureToken { token_span :: !AlexSpan }
   -- Needed, not sure what they are though.  
   | GetToken { token_span :: !AlexSpan }
   | SetToken { token_span :: !AlexSpan }
   
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
