{-# OPTIONS  #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.JavaScript.ParserMonad
-- Copyright   : (c) 2012 Alan Zimmerman
-- License     : BSD-style
-- Stability   : experimental
-- Portability : ghc
--
-- Monad support for JavaScript parser and lexer.
-----------------------------------------------------------------------------

module Language.JavaScript.Parser.ParserMonad
       (
         AlexUserState(..)
       , alexInitUserState
       ) where

import Language.JavaScript.Parser.Token
import Language.JavaScript.Parser.SrcLocation

data AlexUserState = AlexUserState
  {
    previousToken :: !Token  -- the previous token
  , comments :: [Token]      -- accumulated comments
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
   {
     previousToken = initToken
   , comments = []
   }

initToken :: Token
initToken = CommentToken tokenPosnEmpty "" []

