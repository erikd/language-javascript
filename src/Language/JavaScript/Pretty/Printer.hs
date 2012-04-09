module Language.JavaScript.Pretty.Printer (
  -- * Printing
  renderJS
  , renderToString
  ) where

import Data.Char
import Data.List
import Data.Monoid (Monoid, mappend, mempty, mconcat)
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.Parser
import Language.JavaScript.Parser.SrcLocation
import Language.JavaScript.Parser.Token
import qualified Blaze.ByteString.Builder as BB
import qualified Blaze.ByteString.Builder.Char.Utf8 as BS
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as S8
import qualified Data.ByteString.UTF8 as UB
import qualified Codec.Binary.UTF8.String as US

import Debug.Trace

-- ---------------------------------------------------------------------

data Foo = Foo (Int,Int) BB.Builder

-- ---------------------------------------------------------------------
-- Pretty printer stuff via blaze-builder

(<>) :: BB.Builder -> BB.Builder -> BB.Builder
(<>) a b = mappend a b

(<+>) :: BB.Builder -> BB.Builder -> BB.Builder
(<+>) a b = mconcat [a, (text " "), b]

--() ((Int, Int), BB.Builder) -> ((Int, Int), BB.Builder) -> ((Int, Int), BB.Builder)
--() a b =

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

(<!>) :: Foo -> Foo -> Foo
(<!>) (Foo _ bb1) (Foo (r,c) bb2) = Foo (r,c) (mappend bb1 bb2)
--(<!>) a b = mappend a b

-- ---------------------------------------------------------------------

renderJS :: JSNode -> BB.Builder
renderJS node = bb
  where
    Foo _ bb = rn node (Foo (1,1) empty)

-- Take in the current
-- rn :: (Int, Int) -> JSNode -> ((Int, Int), BB.Builder)
rn :: JSNode -> Foo -> Foo

-- Terminals
rn (NS (JSIdentifier s     ) p cs) foo = rcs cs p s  foo
rn (NS (JSDecimal i        ) p cs) foo = rcs cs p i foo
rn (NS (JSLiteral l        ) p cs) foo = rcs cs p l foo
rn (NS (JSHexInteger i     ) p cs) foo = rcs cs p i foo
rn (NS (JSStringLiteral s l) p cs) foo = rcs cs p ((s:l)++[s]) foo
rn (NS (JSRegEx s          ) p cs) foo = rcs cs p s foo

-- Non-Terminals
rn (NS (JSArguments lb xs rb) p [])                    foo = rJS ([lb] ++ xs ++ [rb]) foo
rn (NS (JSArrayLiteral lb xs rb) p [])                 foo = rJS ([lb] ++ xs ++ [rb]) foo
rn (NS (JSBlock lb x rb) p [])                         foo = rJS (lb ++ [x] ++ rb) foo
rn (NS (JSBreak b x1s as) p [])                        foo = rJS ([b]++x1s++[as]) foo
rn (NS (JSCallExpression s os xs cs) p [])             foo = rJS (os ++ xs ++ cs) foo
rn (NS (JSCase ca x1 c x2) p [])                       foo = rJS [ca,x1,c,x2] foo
rn (NS (JSCatch c lb x1 x2s rb x3) p [])               foo = rJS ([c,lb,x1]++x2s++[rb,x3]) foo
rn (NS (JSContinue c xs as) p [])                      foo = rJS ([c]++xs++[as]) foo
rn (NS (JSDefault d c x) p [])                         foo = rJS [d,c,x] foo
rn (NS (JSDoWhile d x1 w lb x2 rb x3) p [])            foo = rJS ([d,x1,w,lb,x2,rb,x3]) foo
rn (NS (JSElision c) p cs)                             foo = rJS [c] foo
rn (NS (JSExpression xs) p cs)                         foo = rJS xs foo
rn (NS (JSExpressionBinary s lhs op rhs) p [])         foo = rJS (lhs ++ [op] ++ rhs) foo
rn (NS (JSExpressionParen lb e rb) p [])               foo = rJS ([lb,e,rb]) foo
rn (NS (JSExpressionPostfix s xs op) p [])             foo = rJS (xs ++ [op]) foo
rn (NS (JSExpressionTernary cond h v1 c v2) p [])      foo = rJS (cond ++[h] ++ v1 ++ [c] ++ v2) foo
rn (NS (JSFinally f x) p [])                           foo = rJS [f,x] foo
rn (NS (JSFor f lb x1s s1 x2s s2 x3s rb x4) p [])      foo = rJS ([f,lb]++x1s++[s1]++x2s++[s2]++x3s++[rb,x4]) foo
rn (NS (JSForIn f lb x1s i x2 rb x3) p [])             foo = rJS ([f,lb]++x1s++[i,x2,rb,x3]) foo
rn (NS (JSForVar f lb v x1s s1 x2s s2 x3s rb x4) p []) foo = rJS ([f,lb,v]++x1s++[s1]++x2s++[s2]++x3s++[rb,x4]) foo
rn (NS (JSForVarIn f lb v x1 i x2 rb x3) p [])         foo = rJS [f,lb,v,x1,i,x2,rb,x3] foo
rn (NS (JSFunction f x1 lb x2s rb lb2 x3 rb2) p [])    foo = rJS ([f,x1,lb]++x2s++[rb,lb2,x3,rb2]) foo
rn (NS (JSFunctionBody xs) p [])                       foo = rJS xs foo
rn (NS (JSFunctionExpression f x1s lb x2s rb lb2 x3 rb2) p []) foo = rJS ([f] ++ x1s ++ [lb] ++ x2s ++ [rb,lb2,x3,rb2]) foo
rn (NS (JSIf i lb x1 rb x2 x3s) p [])                  foo = rJS ([i,lb,x1,rb,x2]++x3s) foo
rn (NS (JSLabelled l c v) p cs)                        foo = rJS [l,c,v] foo
rn (NS (JSMemberDot xs dot n) p [])                    foo = rJS (xs ++ [dot,n]) foo
rn (NS (JSMemberSquare xs lb e rb) p [])               foo = rJS (xs ++ [lb,e,rb]) foo
rn (NS (JSObjectLiteral lb xs rb) p [])                foo = rJS ([lb] ++ xs ++ [rb]) foo
rn (NS (JSOperator n) p cs)                            foo = rJS [n] foo
rn (NS (JSPropertyAccessor s n lb1 ps rb1 lb2 b rb2) p []) foo = rJS ([s,n,lb1] ++ ps ++ [rb1,lb2,b,rb2]) foo
rn (NS (JSPropertyNameandValue n colon vs) p [])       foo = rJS ([n,colon] ++ vs) foo
rn (NS (JSReturn r xs as) p [])                        foo = rJS ([r] ++ xs ++ [as]) foo
rn (NS (JSSourceElements    xs) p cs)                  foo = rJS xs foo
rn (NS (JSSourceElementsTop xs) p cs)                  foo = rJS xs foo
rn (NS (JSStatementBlock lb x rb) p [])                foo = rJS [lb,x,rb] foo
rn (NS (JSStatementList xs) p cs)                      foo = rJS xs foo
rn (NS (JSSwitch s lb x rb x2s) p [])                  foo = rJS ([s,lb,x,rb]++x2s) foo
rn (NS (JSThrow t x) p [])                             foo = rJS [t,x] foo
rn (NS (JSTry t x1 x2s) _p [])                         foo = rJS ([t,x1]++x2s) foo
rn (NS (JSUnary l n) p cs)                             foo = rJS [n] foo
rn (NS (JSVarDecl x1 x2s) p [])                        foo = rJS ([x1]++x2s) foo
rn (NS (JSVariables n xs as) p [])                     foo = rJS ([n]++xs++[as]) foo
rn (NS (JSWhile w lb x1 rb x2) p [])                   foo = rJS [w,lb,x1,rb,x2] foo
rn (NS (JSWith w lb x1 rb x2s) p [])                   foo = rJS ([w,lb,x1,rb]++x2s) foo

-- Debug helper
rn what foo = rs (show what) foo

--rn _ _ = undefined

-- ---------------------------------------------------------------------
-- Helper functions

-- ---------------------------------------------------------------------
-- Need a function that
-- a) renders all comments, according to their positions
-- b) advances to the position of the required string
-- c) renders the string, advancing the position
rcs :: [CommentAnnotation] -> TokenPosn -> String -> Foo -> Foo
rcs cs p s foo = rps p s (rc cs foo)

rc :: [CommentAnnotation] -> Foo -> Foo
rc cs foo = foldl' go foo cs
  where
    go :: Foo -> CommentAnnotation -> Foo
    go foo NoComment = foo
    go foo (CommentA p s) = rps p s foo

-- Render a string at the given position
rps :: TokenPosn -> String -> Foo -> Foo
rps p s foo = (rs s foo')
  where
    foo' = (goto p foo)

-- Render a string
rs :: String -> Foo -> Foo
rs s (Foo (r,c) bb) = (Foo (r',c') (bb <> (text s)))
  where
    (r',c') = foldl' (\(row,col) char -> go (row,col) char) (r,c) s

    go (r,c) '\n' = (r+1,1)
    go (r,c) _    = (r,c+1)


goto :: TokenPosn -> Foo -> Foo
goto (TokenPn _ ltgt ctgt) (Foo (lcur,ccur) bb) = (Foo (lnew,cnew) (bb <> bb'))
-- goto (TokenPn _ ltgt ctgt) (Foo (lcur,ccur) bb) = trace ("goto " ++ (show $ (ltgt,ctgt)) ++ "," ++ (show $ (lcur,ccur)) ++ "," ++ (show $ (lnew,cnew)) ) $  (Foo (lnew,cnew) (bb <> bb'))
  where
    (bbline,ccur') = if (lcur < ltgt) then (text $ (take (ltgt - lcur) $ repeat '\n'),1) else (mempty,ccur)
    bbcol  = if (ccur' < ctgt) then (text $ take (ctgt - ccur') $ repeat ' ' ) else mempty
    bb' = bbline <> bbcol
    lnew = if (lcur < ltgt) then ltgt else lcur
    cnew = if (ccur' < ctgt) then ctgt else ccur'


rJS :: [JSNode] -> Foo -> Foo
rJS xs foo = foldl' (flip rn) foo xs

renderToString :: JSNode -> String
--renderToString js = map (\x -> chr (fromIntegral x)) $ LB.unpack $ BB.toLazyByteString $ renderJS js
renderToString js = US.decode $ LB.unpack $ BB.toLazyByteString $ renderJS js
--renderToString js = lbToStr $ S8.fromChunks [ BB.toLazyByteString $ renderJS js ]
--renderToString js = lbToStr $ BB.toByteString $ renderJS js



-- ---------------------------------------------------------------------
-- Test stuff

_r :: JSNode -> String
_r js = map (\x -> chr (fromIntegral x)) $ LB.unpack $ BB.toLazyByteString $ renderJS js

_t :: String -> String
_t str = _r $ readJs str


-- readJs "/*a*/x"
_ax = (NS
     (JSExpression
       [NS
        (JSIdentifier "x")
        (TokenPn 5 1 6)
        [CommentA (TokenPn 0 1 1) "/*a*/"]])
     (TokenPn 5 1 6)
     [])


-- readJs "//j\nthis_"
-- NS (JSSourceElementsTop [NS (JSExpression [NS (JSIdentifier "this_") (TokenPn 4 2 1) [CommentA (TokenPn 0 1 1) "//j"]]) (TokenPn 4 2 1) []]) (TokenPn 4 2 1) []

_r1 = NS
      (
        JSExpression
          [
            NS
              (JSIdentifier "this_")
              (TokenPn 4 2 1)
              [CommentA (TokenPn 0 1 1) "//j"]
          ])
      (TokenPn 4 2 1) []

-- EOF

