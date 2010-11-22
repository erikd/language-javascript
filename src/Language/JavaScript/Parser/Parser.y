
{
module Language.JavaScript.Parser.Parser where

import Control.Monad.Error.Class (throwError)
import Data.Char
import Language.JavaScript.Parser.Lexer
import Language.JavaScript.Parser.ParserMonad
import Language.JavaScript.Parser.SrcLocation

}

-- The name of the generated function to be exported from the module
%name parse
%tokentype { Token }
%error { parseError }
%monad { P } { thenP } { returnP }
%lexer { lexCont } { EOFToken {} }


%token 

      let             { TokenLet {} }
      in              { TokenIn {} }
      int             { TokenInt {} {-$$-} }
      var             { TokenVar {} {-$$-} }
      '='             { TokenEq {} }
      '+'             { TokenPlus {} }
      '-'             { TokenMinus {} }
      '*'             { TokenTimes {} }
      '/'             { TokenDiv {} }
      '('             { TokenOB {} }
      ')'             { TokenCB {} }

%%

Foo : '(' '+' ')' {}

{-
Exp   : let var '=' Exp in Exp  { Let $2 $4 $6 }
      | Exp1                    { Exp1 $1 }

Exp1  : Exp1 '+' Term           { Plus $1 $3 }
      | Exp1 '-' Term           { Minus $1 $3 }
      | Term                    { Term $1 }

Term  : Term '*' Factor         { Times $1 $3 }
      | Term '/' Factor         { Div $1 $3 }
      | Factor                  { Factor $1 }

Factor			  
      : int                     { Int $1 }
      | var                     { Var $1 }
      | '(' Exp ')'             { Brack $2 }
-}

{

parseError :: Token -> P a 
parseError = throwError . UnexpectedToken 


data Exp  
      = Let String Exp Exp
      | Exp1 Exp1
      deriving Show

data Exp1 
      = Plus Exp1 Term 
      | Minus Exp1 Term 
      | Term Term
      deriving Show

data Term 
      = Times Term Factor 
      | Div Term Factor 
      | Factor Factor
      deriving Show

data Factor 
      = Int Int 
      | Var String 
      | Brack Exp
      deriving Show



}


