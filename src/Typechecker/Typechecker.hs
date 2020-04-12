module Typechecker.Typechecker where

import Data.Maybe (maybeToList)
import Language.JavaScript.Parser.AST

-- types of thing
data Unknown
  = Binding String
  | FunctionReturn String
  deriving (Eq, Ord, Show)

getTypes :: JSAST -> [Unknown]
getTypes (JSAstProgram statements _) = concatMap getTypesFromStatement statements
getTypes _ = []

getTypesFromStatement :: JSStatement -> [Unknown]
getTypesFromStatement a = case a of
  JSConstant _ exp' _ -> maybeToList (fromCommaList exp')
  -- JSReturn _ (maybeExp') _ -> []
  _ -> []

fromCommaList :: JSCommaList JSExpression -> Maybe Unknown
fromCommaList (JSLOne a) = case a of
  JSIdentifier _ name -> Just (Binding name)
  _ -> Nothing
fromCommaList _ = Nothing
