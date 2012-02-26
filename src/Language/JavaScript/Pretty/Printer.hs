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

--rn (JSSourceElements xs)   = rJS (map fixBlock $ fixSourceElements xs)
rn (JSSourceElements xs)   = rJS (fixSourceElements $ map fixBlock xs)

--rn (JSSourceElementsTop xs)= rJS (fixTop $ map fixBlock $ fixSourceElements xs)
rn (JSSourceElementsTop xs)= rJS (fixTop $ fixSourceElements $ map fixBlock xs)


rn (JSFunction s p xs)     = (text "function") <+> (renderJS s) <> (text "(") <> (commaList p) <> (text ")") <> (renderJS xs)
rn (JSFunctionBody xs)     = (text "{") <> (rJS xs) <> (text "}")
rn (JSFunctionExpression [] p xs) = (text "function")             <> (text "(") <> (commaList p) <> (text ")") <> (renderJS xs)
rn (JSFunctionExpression  s p xs) = (text "function") <+> (rJS s) <> (text "(") <> (commaList p) <> (text ")") <> (renderJS xs)
rn (JSArguments xs)        = (text "(") <> (commaListList $ map fixLiterals xs) <> (text ")")

rn (JSBlock x)             = (text "{") <> (renderJS x) <> (text "}")

rn (JSIf c (NS (JSLiteral ";") _ _))= (text "if") <> (text "(") <> (renderJS c) <> (text ")")
rn (JSIf c t)                     = (text "if") <> (text "(") <> (renderJS c) <> (text ")") <> (renderJS $ fixBlock t)

rn (JSIfElse c t (NS (JSLiteral ";") _ _)) = (text "if") <> (text "(") <> (renderJS c) <> (text ")")  <> (renderJS t)
                                   <> (text "else")
rn (JSIfElse c t e)        = (text "if") <> (text "(") <> (renderJS c) <> (text ")") <> (renderJS t)
                                   <> (text "else") <> (spaceOrBlock $ fixBlock e)
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
                                         <> (rJS e2) <> (char ';') <> (rJS e3) <> (char ')') <> (renderJS $ fixBlock s)
rn (JSForIn e1 e2 s)             = (text "for") <> (char '(') <> (rJS e1) <+> (text "in")
                                         <+> (renderJS e2) <> (char ')') <> (renderJS $ fixBlock s)
rn (JSForVar e1 e2 e3 s)         = (text "for") <> (char '(') <> (text "var") <+> (commaList e1) <> (char ';')
                                         <> (rJS e2) <> (char ';') <> (rJS e3) <> (char ')') <> (renderJS $ fixBlock s)
rn (JSForVarIn e1 e2 s)          = (text "for") <> (char '(') <> (text "var") <+> (renderJS e1) <+> (text "in")
                                         <+> (renderJS e2) <> (char ')') <> (renderJS $ fixBlock s)

rn (JSHexInteger i)              = (text $ show i) -- TODO: need to tweak this
rn (JSLabelled l v)              = (renderJS l) <> (text ":") <> (rJS $ fixSourceElements [fixBlock v])
rn (JSObjectLiteral xs)          = (text "{") <> (commaList xs) <> (text "}")
rn (JSPropertyAccessor s n ps b) = (text s) <+> (renderJS n) <> (char '(') <> (rJS ps) <> (text ")") <> (renderJS b)
rn (JSPropertyNameandValue n vs) = (renderJS n) <> (text ":") <> (rJS vs)
rn (JSRegEx s)                   = (text s)

rn (JSReturn [])                 = (text "return")
rn (JSReturn [(NS (JSLiteral ";") _ _)])    = (text "return;")
rn (JSReturn xs)                 = (text "return") <> (if (spaceNeeded xs) then (text " ") else (empty)) <> (rJS $ fixSourceElements xs)

rn (JSThrow e)                   = (text "throw") <+> (renderJS e)

rn (JSStatementBlock x)          = (text "{") <> (renderJS x) <> (text "}")

rn (JSStatementList xs)          = rJS (fixSourceElements $ map fixBlock xs)

rn (JSSwitch e xs)               = (text "switch") <> (char '(') <> (renderJS e) <> (char ')') <>
                                         (char '{') <> (rJS $ fixSemis xs)  <> (char '}')
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


fixTop :: [JSNode] -> [JSNode]
fixTop [] = []
fixTop xs = if (n == (JSLiteral ";")) then (init xs) else (xs)
  where
    (NS n _ _) = last xs

-- Fix semicolons in output of sourceelements and statementlist

fixSourceElements :: [JSNode] -> [JSNode]
fixSourceElements xs = fixSemis $ myFix xs

myFix :: [JSNode] -> [JSNode]
myFix []      = []

-- Sort out empty IF statements
myFix ((NS (JSIf c (NS (JSStatementBlock (NS (JSStatementList []) s1 c1)) s2 c2)) _s3 _):xs) = (NS (JSIf c (NS (JSLiteral "") s1 c1)) s2 c2) : myFix (xs)

myFix [x]     = [x]

myFix (x:(NS (JSFunction v1 v2 v3) s1 c1):xs)  = x : (NS (JSLiteral "\n") s1 c1) : myFix ((NS (JSFunction v1 v2 v3) s1 c1) : xs)
-- Messy way, but it works, until the 3rd element arrives ..
-- TODO: JSStatementBlock.  Damn.
myFix ((NS (JSExpression x) s1 c1):(NS (JSExpression y) s2 c2):xs) = (NS (JSExpression x) s1 c1):(NS (JSLiteral ";") s1 c1):myFix ((NS (JSExpression y) s2 c2):xs)
myFix ((NS (JSExpression x) s1 c1):(NS (JSBlock y) s2 c2):xs)      = (NS (JSExpression x) s1 c1):(NS (JSLiteral ";") s1 c1):myFix ((NS (JSBlock y) s2 c2):xs)
myFix ((NS (JSBlock x) s1 c1)     :(NS (JSBlock y) s2 c2):xs)      = (NS (JSBlock x) s1 c1)     :(NS (JSLiteral ";") s1 c1):myFix ((NS (JSBlock y) s2 c2):xs)
myFix ((NS (JSBlock x) s1 c1)     :(NS (JSExpression y) s2 c2):xs) = (NS (JSBlock x) s1 c1)     :(NS (JSLiteral ";") s1 c1):myFix ((NS (JSExpression y) s2 c2):xs)

myFix ((NS (JSExpression x) s1 c1):(NS (JSStatementBlock y) s2 c2):xs)      =
  (NS (JSExpression x) s1 c1):(NS (JSLiteral ";") s1 c1):myFix ((NS (JSStatementBlock y) s2 c2):xs)
myFix ((NS (JSStatementBlock x) s1 c1)     :(NS (JSStatementBlock y) s2 c2):xs)      =
  (NS (JSStatementBlock x) s1 c1)     :(NS (JSLiteral ";") s1 c1):myFix ((NS (JSStatementBlock y) s2 c2):xs)
myFix ((NS (JSStatementBlock x) s1 c1)     :(NS (JSExpression y) s2 c2):xs) =
  (NS (JSStatementBlock x) s1 c1)     :(NS (JSLiteral ";") s1 c1):myFix ((NS (JSExpression y) s2 c2):xs)

-- Merge adjacent variable declarations, but only of the same type
myFix ((NS (JSVariables t1 x1s) s1 c1):(NS (JSLiteral l) s2 c2):(NS (JSVariables t2 x2s) s3 c3):xs)
  | t1 == t2 = myFix ((NS (JSVariables t1 (x1s++x2s)) s1 c1):xs)
  | otherwise = (NS (JSVariables t1 x1s) s1 c1):myFix ((NS (JSLiteral l) s2 c2):(NS (JSVariables t2 x2s) s3 c3):xs)

myFix ((NS (JSVariables t1 x1s) s1 c1):(NS (JSVariables t2 x2s) s2 c2):xs)
  | t1 == t2 = myFix ((NS (JSVariables t1 (x1s++x2s)) s1 c1):xs)
  | otherwise = (NS (JSVariables t1 x1s) s1 c1):myFix ((NS (JSVariables t2 x2s) s2 c2):xs)

-- Merge adjacent semi colons
myFix ((NS (JSLiteral ";") s1 c1):(NS (JSLiteral ";") _s2 _c2):xs)  = myFix ((NS (JSLiteral ";") s1 c1):xs)
myFix ((NS (JSLiteral ";") s1 c1):(NS (JSLiteral "" ) _s2 _c2):xs)  = myFix ((NS (JSLiteral "" ) s1 c1):xs)


myFix (x:xs)  = x : myFix xs

-- Merge strings split over lines and using "+"
fixLiterals :: [JSNode] -> [JSNode]
fixLiterals [] = []
-- Old version
fixLiterals ((NS (JSStringLiteral d1 s1) ss1 c1):(NS (JSExpressionBinary "+" [(NS (JSStringLiteral d2 s2) ss2 c2)] r) ss3 c3):xs)
       | d1 == d2 = fixLiterals ((NS (JSStringLiteral d1 (s1++s2)) ss1 c1):(r++xs))
       | otherwise = (NS (JSStringLiteral d1 s1) ss1 c1):fixLiterals ((NS (JSExpressionBinary "+" [(NS (JSStringLiteral d2 s2) ss2 c2)] r) ss3 c3):xs)

fixLiterals ((NS (JSExpressionBinary "+" [(NS (JSStringLiteral d1 s1) ss2 c2)] [(NS (JSStringLiteral d2 s2) ss3 c3)]) ss4 c4):xs)
       | d1 == d2 = fixLiterals ((NS (JSStringLiteral d1 (s1++s2)) ss2 c2):xs)
       | otherwise = (NS (JSExpressionBinary "+" [(NS (JSStringLiteral d1 s1) ss2 c2)] [(NS (JSStringLiteral d2 s2) ss3 c3)]) ss4 c4):fixLiterals xs

fixLiterals (x:xs) = x:fixLiterals xs

-- Sort out Semicolons
fixSemis :: [JSNode] -> [JSNode]
--fixSemis xs = fixSemis' $ filter (\x -> x /= JSLiteral ";" && x /= JSLiteral "") xs
fixSemis xs = fixSemis' $ filter (\(NS x _ _) -> x /= JSLiteral ";" && x /= JSLiteral "") xs

fixSemis' :: [JSNode] -> [JSNode]
fixSemis' [] = []
fixSemis' [(NS (JSContinue [(NS (JSLiteral ";") _ _)]) s2 c2)] = [(NS (JSContinue []) s2 c2)]
fixSemis' [x] = [x]
fixSemis' ((NS (JSIf c (NS (JSReturn [(NS (JSLiteral ";") s1 c1)]) s2 c2) ) s3 c3):xs)  =
  (NS (JSIf c (NS (JSReturn [(NS (JSLiteral ";") s1 c1)]) s2 c2)) s3 c3):(fixSemis' xs)
fixSemis' ((NS (JSIf c (NS (JSContinue [(NS (JSLiteral ";") s1 c1)]) s2 c2) ) s3 c3):xs)    =
  (NS (JSIf c (NS (JSContinue [(NS (JSLiteral ";") s1 c1)]) s2 c2)) s3 c3):(fixSemis' xs)
fixSemis' (x:(NS (JSLiteral "\n") s1 c1):xs) = x:(NS (JSLiteral "\n") s1 c1):(fixSemis' xs) -- TODO: is this needed?
fixSemis' ((NS (JSCase e1 ((NS (JSStatementList []) s1 c1))) s2 c2):(NS (JSCase e2 x) s3 c3):xs) =
  (NS (JSCase e1 ((NS (JSStatementList []) s1 c1))) s2 c2):fixSemis' ((NS (JSCase e2 x) s3 c3):xs)
fixSemis' (x:xs) = x:(NS (JSLiteral ";") tokenPosnEmpty []):fixSemis' xs

-- Remove extraneous braces around blocks
fixBlock :: JSNode -> JSNode

fixBlock (NS (JSBlock          (NS (JSStatementList []) s1 c1) ) _ _) = (NS (JSLiteral ";") s1 c1)
fixBlock (NS (JSStatementBlock (NS (JSStatementList []) s1 c1) ) _ _) = (NS (JSLiteral ";") s1 c1)

fixBlock (NS (JSBlock          (NS (JSStatementList [x]) _ _) ) _ _) = fixBlock x
fixBlock (NS (JSStatementBlock (NS (JSStatementList [x]) _ _) ) _ _) = fixBlock x

fixBlock (NS (JSBlock (NS (JSStatementList xs) s1 c1) ) s2 c2) =
  fixBlock' (NS (JSBlock (NS (JSStatementList (fixSourceElements xs)) s1 c1)) s2 c2)

fixBlock (NS (JSStatementBlock (NS (JSStatementList xs) s1 c1) ) s2 c2) =
  fixBlock' (NS (JSStatementBlock (NS (JSStatementList (fixSourceElements xs)) s1 c1)) s2 c2)

fixBlock x = x

fixBlock' :: JSNode -> JSNode
fixBlock' (NS (JSBlock          (NS (JSStatementList [x]) _ _)) _ _) = x
fixBlock' (NS (JSStatementBlock (NS (JSStatementList [x]) _ _)) _ _) = x
--fixBlock' (JSBlock (JSStatementList [x,JSLiteral ""])) = x -- TODO: fix parser to not emit this case
fixBlock' x = x

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

