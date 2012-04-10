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
   , CommentAnnotation(..)
   -- * String conversion
   , debugTokenString
   -- * Classification
   -- TokenClass (..),
   ) where

import Data.Data
import Language.JavaScript.Parser.SrcLocation

data CommentAnnotation = CommentA TokenPosn String
                       | WhiteSpace TokenPosn String
                       | NoComment
   deriving (Eq,{-Ord,-}Show,Typeable,Data,Read)

-- | Lexical tokens.
-- Each may be annotated with any comment occuring between the prior token and this one
data Token
   -- Comment
   = CommentToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation] } -- ^ Single line comment.
   | WsToken      { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation] } -- ^ White space, for preservation.

   -- Identifiers
   | IdentifierToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }            -- ^ Identifier.

   -- Javascript Literals

   | DecimalToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]   }
     -- ^ Literal: Decimal
   | HexIntegerToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]   }
     -- ^ Literal: Hexadecimal Integer
   | StringToken { token_span :: !TokenPosn, token_literal :: !String, token_delimiter :: !Char, token_comment :: ![CommentAnnotation]  }
     -- ^ Literal: string, delimited by either single or double quotes
   | RegExToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]   }
     -- ^ Literal: Regular Expression

   -- Keywords
   | BreakToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | CaseToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | CatchToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | ConstToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | ContinueToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | DebuggerToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | DefaultToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | DeleteToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | DoToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | ElseToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | EnumToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | FalseToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | FinallyToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | ForToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | FunctionToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | IfToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | InToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | InstanceofToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | NewToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | NullToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | ReturnToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | SwitchToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | ThisToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | ThrowToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | TrueToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | TryToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | TypeofToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | VarToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | VoidToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | WhileToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | WithToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   -- Future reserved words
   | FutureToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   -- Needed, not sure what they are though.
   | GetToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | SetToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }

   -- Delimiters
   -- Operators
   | SemiColonToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | CommaToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | HookToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | ColonToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | OrToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | AndToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | BitwiseOrToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | BitwiseXorToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | BitwiseAndToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | StrictEqToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | EqToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | AssignToken { token_span :: !TokenPosn, token_literal :: !String, token_comment :: ![CommentAnnotation]  }
   | SimpleAssignToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | StrictNeToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | NeToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | LshToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | LeToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | LtToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | UrshToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | RshToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | GeToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | GtToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | IncrementToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | DecrementToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | PlusToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | MinusToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | MulToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | DivToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | ModToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | NotToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | BitwiseNotToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | DotToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | LeftBracketToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | RightBracketToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | LeftCurlyToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | RightCurlyToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | LeftParenToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | RightParenToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }
   | CondcommentEndToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }

   -- Special cases
   | TailToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  } -- ^ Stuff between last JS and EOF
   | EOFToken { token_span :: !TokenPosn, token_comment :: ![CommentAnnotation]  }  -- ^ End of file
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
