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

bp :: (Int,Int) -> TokenPosn -> ((Int,Int) -> ((Int,Int),BB.Builder)) -> ((Int,Int),BB.Builder)
bp (r,c) p f = ((r'',c''),bb <> bb')
  where
    ((r',c'),bb) = skipTo (r,c) p
    ((r'',c''),bb') = f (r',c')

bprJS
  :: (Int, Int) -> TokenPosn -> [JSNode] -> ((Int, Int), BB.Builder)
bprJS (r,c) p xs = bp (r,c) p (\(r,c) -> rJS (r,c) xs)

bpText
  :: (Int, Int) -> TokenPosn -> [Char] -> ((Int, Int), BB.Builder)
bpText (r,c) p s = bp (r,c) p (\(r,c) -> ((r,c + (length s)),text s))


-- ---------------------------------------------------------------------

renderJS :: JSNode -> BB.Builder
renderJS node = bb
  where
    (_,bb) = rn (1,1) node

-- Take in the current
rn :: (Int,Int) -> JSNode -> ((Int,Int), BB.Builder)
{-
rn (r,c) (NS (JSEmpty l) p cs) = do
  (r',c') <- skipTo (r,c) p
  return (rn (r',c') l)
-}
rn (r,c) (NS (JSSourceElementsTop xs) p cs) = bprJS (r,c) p xs
rn (r,c) (NS (JSSourceElements    xs) p cs) = bprJS (r,c) p xs

rn (r,c) (NS (JSExpression xs) p cs)        = bprJS (r,c) p xs

rn (r,c) (NS (JSIdentifier s) p cs)         = bpText (r,c) p s

rn (r,c) (NS (JSOperator s) p cs)           = bpText (r,c) p s

rn (r,c) (NS (JSDecimal i) p cs)            = bpText (r,c) p i

rn (r,c) (NS (JSLiteral l) p cs)            = bpText (r,c) p l

rn (r,c) (NS (JSUnary l) p cs               = bpText (r,c) p l

{-

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
rn (JSStringLiteral s l)   = empty <> (char s) <> (text l) <> (char s)
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
-}
-- Helper functions
rJS :: (Int,Int) -> [JSNode] -> ((Int,Int),BB.Builder)
-- rJS xs = hcat $ map renderJS xs
--rJS (r,c) xs = map rn xs
rJS (r,c) xs = foldl' frn ((r,c),mempty) xs
  where
    frn :: ((Int,Int),BB.Builder) -> JSNode -> ((Int,Int),BB.Builder)
    frn ((rc,cc),bb) n = ((rc',cc'),bb <> bb')
      where
        ((rc',cc'),bb') = rn (rc,cc) n

{-
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
-}


{-

TODO: Collapse this into JSLiteral ";"

JSStatementBlock (JSStatementList [JSStatementBlock (JSStatementList [])])
-}
-- ---------------------------------------------------------------
-- Utility stuff

{-
-- A space is needed if this expression starts with an identifier etc, but not if with a '('
spaceNeeded :: [JSNode] -> Bool
spaceNeeded xs =
  let
   -- str = show $ rJS xs
    str = LB.unpack $ BB.toLazyByteString $ rJS xs
  in
   head str /= (fromIntegral $ ord '(')
-}
skipTo :: (Int,Int) -> TokenPosn -> ((Int,Int), BB.Builder)
skipTo (lcur,ccur) (TokenPn _ ltgt ctgt) = ((lnew,cnew),bb)
  where
    lnew = if (lcur < ltgt) then ltgt else lcur
    cnew = if (ccur < ctgt) then ctgt else ccur
    bbline = if (lcur < ltgt) then (text $ take (ltgt - lcur) $ repeat '\n') else mempty
    bbcol  = if (ccur < ctgt) then (text $ take (ctgt - ccur) $ repeat ' ' ) else mempty
    bb = bbline <> bbcol

-- ---------------------------------------------------------------------
-- Test stuff

_r :: JSNode -> [Char]
_r js = map (\x -> chr (fromIntegral x)) $ LB.unpack $ BB.toLazyByteString $ renderJS js

_t :: String -> String
_t str = _r $ readJs str

-- EOF

