module Language.JavaScript.Pretty.Printer (
  -- * Printing
  renderJS
  , renderToString
  ) where

import Blaze.ByteString.Builder (Builder, toLazyByteString)
import Data.List
import Data.Monoid (Monoid, mappend, mempty)
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.SrcLocation
import Language.JavaScript.Parser.Token
import qualified Blaze.ByteString.Builder.Char.Utf8 as BS
import qualified Data.ByteString.Lazy as LB
import qualified Codec.Binary.UTF8.String as US

-- ---------------------------------------------------------------------

data PosAccum = PA (Int, Int) Builder

-- ---------------------------------------------------------------------
-- Pretty printer stuff via blaze-builder

(<>) :: Builder -> Builder -> Builder
(<>) = mappend

empty :: Builder
empty = mempty

text :: String -> Builder
text = BS.fromString

-- ---------------------------------------------------------------------

renderJS :: JSNode -> Builder
renderJS node = bb
  where
    PA _ bb = rn node (PA (1,1) empty)

-- rn : Render node.
rn :: JSNode -> PosAccum -> PosAccum

-- Terminals
rn (JSIdentifier    (JSAnnot p cs) s  ) foo = rcs cs p s foo
rn (JSDecimal       (JSAnnot p cs) i  ) foo = rcs cs p i foo
rn (JSLiteral       (JSAnnot p cs) l  ) foo = rcs cs p l foo
rn (JSHexInteger    (JSAnnot p cs) i  ) foo = rcs cs p i foo
rn (JSOctal         (JSAnnot p cs) i  ) foo = rcs cs p i foo
rn (JSStringLiteral (JSAnnot p cs) s l) foo = rcs cs p ((s:l)++[s]) foo
rn (JSRegEx         (JSAnnot p cs) s  ) foo = rcs cs p s foo

-- Non-Terminals
rn (JSArguments            JSNoAnnot lb xs rb)                        foo = rJS ([lb] ++ xs ++ [rb]) foo
rn (JSArrayLiteral         JSNoAnnot lb xs rb)                        foo = rJS ([lb] ++ xs ++ [rb]) foo
rn (JSBlock                JSNoAnnot lb x rb)                         foo = rJS (lb ++ x ++ rb) foo
rn (JSBreak                JSNoAnnot b x1s as)                        foo = rJS ([b]++x1s++[as]) foo
rn (JSCallExpression       JSNoAnnot _s os xs cs)                     foo = rJS (os ++ xs ++ cs) foo
rn (JSCase                 JSNoAnnot ca x1 c x2s)                     foo = rJS ([ca,x1,c]++x2s) foo
rn (JSCatch                JSNoAnnot c lb x1 x2s rb x3)               foo = rJS ([c,lb,x1]++x2s++[rb,x3]) foo
rn (JSContinue             JSNoAnnot c xs as)                         foo = rJS ([c]++xs++[as]) foo
rn (JSDefault              JSNoAnnot d c xs)                          foo = rJS ([d,c]++xs) foo
rn (JSDoWhile              JSNoAnnot d x1 w lb x2 rb x3)              foo = rJS [d,x1,w,lb,x2,rb,x3] foo
rn (JSElision              JSNoAnnot c)                               foo = rJS [c] foo
rn (JSExpression           JSNoAnnot xs)                              foo = rJS xs foo
rn (JSExpressionBinary     JSNoAnnot lhs op rhs)                      foo = rJS (lhs ++ [op] ++ rhs) foo
rn (JSExpressionParen      JSNoAnnot lb e rb)                         foo = rJS [lb,e,rb] foo
rn (JSExpressionPostfix    JSNoAnnot xs op)                           foo = rJS (xs ++ [op]) foo
rn (JSExpressionTernary    JSNoAnnot cond h v1 c v2)                  foo = rJS (cond ++[h] ++ v1 ++ [c] ++ v2) foo
rn (JSFinally              JSNoAnnot f x)                             foo = rJS [f,x] foo
rn (JSFor                  JSNoAnnot f lb x1s s1 x2s s2 x3s rb x4)    foo = rJS ([f,lb]++x1s++[s1]++x2s++[s2]++x3s++[rb,x4]) foo
rn (JSForIn                JSNoAnnot f lb x1s i x2 rb x3)             foo = rJS ([f,lb]++x1s++[i,x2,rb,x3]) foo
rn (JSForVar               JSNoAnnot f lb v x1s s1 x2s s2 x3s rb x4)  foo = rJS ([f,lb,v]++x1s++[s1]++x2s++[s2]++x3s++[rb,x4]) foo
rn (JSForVarIn             JSNoAnnot f lb v x1 i x2 rb x3)            foo = rJS [f,lb,v,x1,i,x2,rb,x3] foo
rn (JSFunction             JSNoAnnot f x1 lb x2s rb x3)               foo = rJS ([f,x1,lb]++x2s++[rb,x3]) foo
rn (JSFunctionExpression   JSNoAnnot f x1s lb x2s rb x3)              foo = rJS ([f] ++ x1s ++ [lb] ++ x2s ++ [rb,x3]) foo
rn (JSIf                   JSNoAnnot i lb x1 rb x2s x3s)              foo = rJS ([i,lb,x1,rb]++x2s++x3s) foo
rn (JSLabelled             JSNoAnnot l c v)                           foo = rJS [l,c,v] foo


rn (JSLiteral              JSNoAnnot [])                               foo = rJS [] foo


rn (JSMemberDot            JSNoAnnot xs dot n)                        foo = rJS (xs ++ [dot,n]) foo
rn (JSMemberSquare         JSNoAnnot xs lb e rb)                      foo = rJS (xs ++ [lb,e,rb]) foo
rn (JSObjectLiteral        JSNoAnnot lb xs rb)                        foo = rJS ([lb] ++ xs ++ [rb]) foo
rn (JSOperator             JSNoAnnot n)                               foo = rJS [n] foo
rn (JSPropertyAccessor     JSNoAnnot s n lb1 ps rb1 b)                foo = rJS ([s,n,lb1] ++ ps ++ [rb1,b]) foo
rn (JSPropertyNameandValue JSNoAnnot n colon vs)                      foo = rJS ([n,colon] ++ vs) foo
rn (JSReturn               JSNoAnnot r xs as)                         foo = rJS ([r] ++ xs ++ [as]) foo
rn (JSSourceElementsTop    JSNoAnnot xs)                              foo = rJS xs foo
rn (JSSwitch               JSNoAnnot s lb x rb x2)                    foo = rJS [s,lb,x,rb,x2] foo
rn (JSThrow                JSNoAnnot t x)                             foo = rJS [t,x] foo
rn (JSTry                  JSNoAnnot t x1 x2s)                        foo = rJS ([t,x1]++x2s) foo
rn (JSUnary                JSNoAnnot n)                               foo = rJS [n] foo
rn (JSVarDecl              JSNoAnnot x1 x2s)                          foo = rJS (x1:x2s) foo
rn (JSVariables            JSNoAnnot n xs as)                         foo = rJS ([n]++xs++[as]) foo
rn (JSWhile                JSNoAnnot w lb x1 rb x2)                   foo = rJS [w,lb,x1,rb,x2] foo
rn (JSWith                 JSNoAnnot w lb x1 rb x2s)                  foo = rJS ([w,lb,x1,rb]++x2s) foo

-- Debug helper
rn what foo = rs ("X " ++ show what ++ " X") foo

-- ---------------------------------------------------------------------
-- Helper functions

-- ---------------------------------------------------------------------
-- Need a function that
-- a) renders all comments, according to their positions
-- b) advances to the position of the required string
-- c) renders the string, advancing the position
rcs :: [CommentAnnotation] -> TokenPosn -> String -> PosAccum -> PosAccum
rcs cs p s foo = rps p s (rc cs foo)

rc :: [CommentAnnotation] -> PosAccum -> PosAccum
rc cs foo = foldl' go foo cs
  where
    go :: PosAccum -> CommentAnnotation -> PosAccum
    go foo' NoComment = foo'
    go foo' (CommentA   p s) = rps p s foo'
    go foo' (WhiteSpace p s) = rps p s foo'

-- Render a string at the given position
rps :: TokenPosn -> String -> PosAccum -> PosAccum
rps p s foo = rs s foo'
  where
    foo' = goto p foo

-- Render a string
rs :: String -> PosAccum -> PosAccum
rs s (PA (r,c) bb) = PA (r',c') (bb <> text s)
  where
    (r',c') = foldl' (\(row,col) ch -> go (row,col) ch) (r,c) s

    go (rx,_)  '\n' = (rx+1,1)
    go (rx,cx) '\t' = (rx,cx+8)
    go (rx,cx) _    = (rx,cx+1)


goto :: TokenPosn -> PosAccum -> PosAccum
goto (TokenPn _ ltgt ctgt) (PA (lcur,ccur) bb) = PA (lnew,cnew) (bb <> bb')
  where
    (bbline,ccur') = if lcur < ltgt then (text (replicate (ltgt - lcur) '\n'),1) else (mempty,ccur)
    bbcol  = if ccur' < ctgt then text (replicate (ctgt - ccur') ' ') else mempty
    bb' = bbline <> bbcol
    lnew = if lcur < ltgt then ltgt else lcur
    cnew = if ccur' < ctgt then ctgt else ccur'


rJS :: [JSNode] -> PosAccum -> PosAccum
rJS xs foo = foldl' (flip rn) foo xs

renderToString :: JSNode -> String
-- need to be careful to not lose the unicode encoding on output
renderToString js = US.decode $ LB.unpack $ toLazyByteString $ renderJS js


-- EOF

