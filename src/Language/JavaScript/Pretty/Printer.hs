module Language.JavaScript.Pretty.Printer (
  -- * Printing
  renderJS
  ) where

import Data.Char
import Data.List
import Data.Monoid (Monoid, mappend, mempty, mconcat)
-- import Text.Jasmine.Parse
import Language.JavaScript.Parser
import qualified Blaze.ByteString.Builder as BB
import qualified Blaze.ByteString.Builder.Char.Utf8 as BS
import qualified Data.ByteString.Lazy as LB

-- ---------------------------------------------------------------------
-- Pretty printer stuff via blaze-builder

(<>) :: BB.Builder -> BB.Builder -> BB.Builder
(<>) a b = mappend a b

(<+>) :: BB.Builder -> BB.Builder -> BB.Builder
(<+>) a b = mconcat [a, (text " "), b]

hcat :: (Monoid a) => [a] -> a
hcat xs = mconcat xs

empty :: BB.Builder
empty = mempty

text :: String -> BB.Builder
text s = BS.fromString s

char :: Char -> BB.Builder
char c = BS.fromChar c

comma :: BB.Builder
comma = BS.fromChar ','

punctuate :: a -> [a] -> [a]
punctuate p xs = intersperse p xs

-- ---------------------------------------------------------------------

renderJS :: JSNode -> BB.Builder
renderJS (NS node _ _) = rn node

rn :: Node -> BB.Builder
rn (JSEmpty l)             = (renderJS l)
rn (JSIdentifier s)        = text s
rn (JSDecimal i)           = text i
rn (JSOperator s)          = text s
rn (JSExpression xs)       = rJS xs

rn (JSSourceElements xs)   = rJS xs

rn (JSSourceElementsTop xs)= rJS xs


rn (JSFunction s p xs)     = (text "function") <+> (renderJS s) <> (text "(") <> (commaList p) <> (text ")") <> (renderJS xs)
rn (JSFunctionBody xs)     = (text "{") <> (rJS xs) <> (text "}")
rn (JSFunctionExpression [] p xs) = (text "function")             <> (text "(") <> (commaList p) <> (text ")") <> (renderJS xs)
rn (JSFunctionExpression  s p xs) = (text "function") <+> (rJS s) <> (text "(") <> (commaList p) <> (text ")") <> (renderJS xs)
rn (JSArguments xs)        = (text "(") <> (commaListList xs) <> (text ")")

rn (JSBlock x)             = (text "{") <> (renderJS x) <> (text "}")

rn (JSIf c (NS (JSLiteral ";") _ _))= (text "if") <> (text "(") <> (renderJS c) <> (text ")")
rn (JSIf c t)                     = (text "if") <> (text "(") <> (renderJS c) <> (text ")") <> (renderJS  t)

rn (JSIfElse c t (NS (JSLiteral ";") _ _)) = (text "if") <> (text "(") <> (renderJS c) <> (text ")")  <> (renderJS t)
                                   <> (text "else")
rn (JSIfElse c t e)        = (text "if") <> (text "(") <> (renderJS c) <> (text ")") <> (renderJS t)
                                   <> (text "else") <> (spaceOrBlock e)
rn (JSMemberDot xs y)        = (rJS xs) <> (text ".") <> (renderJS y)
rn (JSMemberSquare xs x)   = (rJS xs) <> (text "[") <> (renderJS x) <> (text "]")
rn (JSLiteral l)           = (text l)
rn (JSStringLiteral s l)   = empty <> (char s) <> (text l) <> (char s)
rn (JSUnary l  )           = text l
rn (JSArrayLiteral xs)     = (text "[") <> (rJS xs) <> (text "]")

rn (JSBreak [] [])            = (text "break")
rn (JSBreak [] _xs)           = (text "break") -- <> (rJS xs) -- <> (text ";")
rn (JSBreak is _xs)           = (text "break") <+> (rJS is) -- <> (rJS xs)

rn (JSCallExpression "()" xs) = (rJS xs)
rn (JSCallExpression   t  xs) = (char $ head t) <> (rJS xs) <> (if ((length t) > 1) then (char $ last t) else empty)

-- No space between 'case' and string literal. TODO: what about expression in parentheses?
--rn (JSCase (JSExpression [JSStringLiteral sepa s]) xs) = (text "case") <> (renderJS (JSStringLiteral sepa s))
rn (JSCase (NS (JSExpression [(NS (JSStringLiteral sepa s) s1 c1)]) _ _) xs) = (text "case") <> (renderJS (NS (JSStringLiteral sepa s) s1 c1))
                                                               <> (char ':') <> (renderJS xs)
rn (JSCase e xs)           = (text "case") <+> (renderJS e) <> (char ':') <> (renderJS xs) -- <> (text ";");

rn (JSCatch i [] s)        = (text "catch") <> (char '(') <> (renderJS i) <>  (char ')') <> (renderJS s)
rn (JSCatch i c s)         = (text "catch") <> (char '(') <> (renderJS i) <>
                                   (text " if ") <> (rJS c) <> (char ')') <> (renderJS s)

rn (JSContinue is)         = (text "continue") <> (rJS is) -- <> (char ';')
rn (JSDefault xs)          = (text "default") <> (char ':') <> (renderJS xs)
rn (JSDoWhile s e _ms)     = (text "do") <> (renderJS s) <> (text "while") <> (char '(') <> (renderJS e) <> (char ')') -- <> (renderJS ms)
--rn (JSElementList xs)      = rJS xs
rn (JSElision xs)          = (char ',') <> (rJS xs)
rn (JSExpressionBinary o e1 e2) = (rJS e1) <> (text o) <> (rJS e2)
--rn (JSExpressionBinary o e1 e2) = (text o) <> (rJS e1) <> (rJS e2)
rn (JSExpressionParen e)        = (char '(') <> (renderJS e) <> (char ')')
rn (JSExpressionPostfix o e)    = (rJS e) <> (text o)
rn (JSExpressionTernary c v1 v2) = (rJS c) <> (char '?') <> (rJS v1) <> (char ':') <> (rJS v2)
rn (JSFinally b)                 = (text "finally") <> (renderJS b)

rn (JSFor e1 e2 e3 s)            = (text "for") <> (char '(') <> (commaList e1) <> (char ';')
                                         <> (rJS e2) <> (char ';') <> (rJS e3) <> (char ')') <> (renderJS s)
rn (JSForIn e1 e2 s)             = (text "for") <> (char '(') <> (rJS e1) <+> (text "in")
                                         <+> (renderJS e2) <> (char ')') <> (renderJS s)
rn (JSForVar e1 e2 e3 s)         = (text "for") <> (char '(') <> (text "var") <+> (commaList e1) <> (char ';')
                                         <> (rJS e2) <> (char ';') <> (rJS e3) <> (char ')') <> (renderJS s)
rn (JSForVarIn e1 e2 s)          = (text "for") <> (char '(') <> (text "var") <+> (renderJS e1) <+> (text "in")
                                         <+> (renderJS e2) <> (char ')') <> (renderJS s)

rn (JSHexInteger i)              = (text $ show i) -- TODO: need to tweak this
rn (JSLabelled l v)              = (renderJS l) <> (text ":") <> (rJS  [v])
rn (JSObjectLiteral xs)          = (text "{") <> (commaList xs) <> (text "}")
rn (JSPropertyAccessor s n ps b) = (text s) <+> (renderJS n) <> (char '(') <> (rJS ps) <> (text ")") <> (renderJS b)
rn (JSPropertyNameandValue n vs) = (renderJS n) <> (text ":") <> (rJS vs)
rn (JSRegEx s)                   = (text s)

rn (JSReturn [])                 = (text "return")
rn (JSReturn [(NS (JSLiteral ";") _ _)])    = (text "return;")
rn (JSReturn xs)                 = (text "return") <> (if (spaceNeeded xs) then (text " ") else (empty)) <> (rJS xs)

rn (JSThrow e)                   = (text "throw") <+> (renderJS e)

rn (JSStatementBlock x)          = (text "{") <> (renderJS x) <> (text "}")

rn (JSStatementList xs)          = rJS xs

rn (JSSwitch e xs)               = (text "switch") <> (char '(') <> (renderJS e) <> (char ')') <>
                                         (char '{') <> (rJS xs)  <> (char '}')
rn (JSTry e xs)                  = (text "try") <> (renderJS e) <> (rJS xs)

rn (JSVarDecl i [])              = (renderJS i)
rn (JSVarDecl i xs)              = (renderJS i) <> (text "=") <> (rJS xs)

rn (JSVariables kw xs)           = (text kw) <+> (commaList xs)

rn (JSWhile e (NS (JSLiteral ";") _ _))   = (text "while") <> (char '(') <> (renderJS e) <> (char ')') -- <> (renderJS s)
rn (JSWhile e s)                 = (text "while") <> (char '(') <> (renderJS e) <> (char ')') <> (renderJS s)

rn (JSWith e s)                  = (text "with") <> (char '(') <> (renderJS e) <> (char ')') <> (rJS s)

-- Helper functions
rJS :: [JSNode] -> BB.Builder
rJS xs = hcat $ map renderJS xs

commaList :: [JSNode] -> BB.Builder
commaList [] = empty
commaList xs = (hcat $ (punctuate comma (toDoc xs') ++ trail))
  where
    -- (xs', trail) = if (last xs == JSLiteral ",") then (init xs, [comma]) else (xs,[])
    (xs', trail) = if (x' == JSLiteral ",") then (init xs, [comma]) else (xs,[])
    (NS x' _ _) = last xs

commaListList :: [[JSNode]] -> BB.Builder
commaListList xs = (hcat $ punctuate comma $ map rJS xs)

toDoc :: [JSNode] -> [BB.Builder]
toDoc xs = map renderJS xs

spaceOrBlock :: JSNode -> BB.Builder
spaceOrBlock (NS (JSBlock xs) _ _) = rn (JSBlock xs)
spaceOrBlock (NS (JSStatementBlock xs) _ _) = rn (JSStatementBlock xs)
spaceOrBlock x            = (text " ") <> (renderJS x)


{-

TODO: Collapse this into JSLiteral ";"

JSStatementBlock (JSStatementList [JSStatementBlock (JSStatementList [])])
-}
-- ---------------------------------------------------------------
-- Utility stuff

-- A space is needed if this expression starts with an identifier etc, but not if with a '('
spaceNeeded :: [JSNode] -> Bool
spaceNeeded xs =
  let
   -- str = show $ rJS xs
    str = LB.unpack $ BB.toLazyByteString $ rJS xs
  in
   head str /= (fromIntegral $ ord '(')

-- ---------------------------------------------------------------------
-- Test stuff

_r :: JSNode -> [Char]
_r js = map (\x -> chr (fromIntegral x)) $ LB.unpack $ BB.toLazyByteString $ renderJS js

_t :: String -> String
_t str = _r $ readJs str

-- EOF

