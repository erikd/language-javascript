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

module Language.JavaScript.Parser.Token
    -- * The tokens
    ( Token (..)
    , CommentAnnotation (..)
    -- * String conversion
    , debugTokenString
    -- * Classification
    -- TokenClass (..),
    ) where

import Data.Data
import Language.JavaScript.Parser.SrcLocation

data CommentAnnotation
    = CommentA TokenPosn String
    | WhiteSpace TokenPosn String
    | NoComment
    deriving (Eq, Show, Typeable, Data, Read)

-- | Lexical tokens.
-- Each may be annotated with any comment occuring between the prior token and this one
data Token
    -- Comment
    = CommentToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation] } -- ^ Single line comment.
    | WsToken      { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation] } -- ^ White space, for preservation.

    -- Identifiers
    | IdentifierToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }            -- ^ Identifier.

    -- Javascript Literals

    | DecimalToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]   }
    -- ^ Literal: Decimal
    | HexIntegerToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]   }
    -- ^ Literal: Hexadecimal Integer
    | OctalToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]   }
    -- ^ Literal: Octal Integer
    | StringToken { tokenSpan :: !TokenPosn, token_literal :: !String, token_delimiter :: !Char, tokenComment :: ![CommentAnnotation]  }
    -- ^ Literal: string, delimited by either single or double quotes
    | RegExToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]   }
    -- ^ Literal: Regular Expression

    -- Keywords
    | BreakToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | CaseToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | CatchToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | ConstToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | ContinueToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | DebuggerToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | DefaultToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | DeleteToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | DoToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | ElseToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | EnumToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | FalseToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | FinallyToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | ForToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | FunctionToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | IfToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | InToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | InstanceofToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | NewToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | NullToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | ReturnToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | SwitchToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | ThisToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | ThrowToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | TrueToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | TryToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | TypeofToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | VarToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | VoidToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | WhileToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | WithToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    -- Future reserved words
    | FutureToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    -- Needed, not sure what they are though.
    | GetToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | SetToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }

    -- Delimiters
    -- Operators
    | SemiColonToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | CommaToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | HookToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | ColonToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | OrToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | AndToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | BitwiseOrToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | BitwiseXorToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | BitwiseAndToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | StrictEqToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | EqToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | AssignToken { tokenSpan :: !TokenPosn, token_literal :: !String, tokenComment :: ![CommentAnnotation]  }
    | SimpleAssignToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | StrictNeToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | NeToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | LshToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | LeToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | LtToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | UrshToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | RshToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | GeToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | GtToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | IncrementToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | DecrementToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | PlusToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | MinusToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | MulToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | DivToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | ModToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | NotToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | BitwiseNotToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | DotToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | LeftBracketToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | RightBracketToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | LeftCurlyToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | RightCurlyToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | LeftParenToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | RightParenToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | CondcommentEndToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }

    -- Special cases
    | TailToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  } -- ^ Stuff between last JS and EOF
    | EOFToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }  -- ^ End of file
    deriving (Eq, Show, Typeable)


-- | Produce a string from a token containing detailed information. Mainly intended for debugging.
debugTokenString :: Token -> String
debugTokenString = takeWhile (/= ' ') . show
