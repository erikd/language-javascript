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


class RenderJS a where
    -- Render node.
    rn :: a -> PosAccum -> PosAccum

instance RenderJS JSNode where
    -- Terminals
    rn (JSIdentifier    (JSAnnot p cs) s  ) pacc = rcs cs p s pacc
    rn (JSDecimal       (JSAnnot p cs) i  ) pacc = rcs cs p i pacc
    rn (JSLiteral       (JSAnnot p cs) l  ) pacc = rcs cs p l pacc
    rn (JSHexInteger    (JSAnnot p cs) i  ) pacc = rcs cs p i pacc
    rn (JSOctal         (JSAnnot p cs) i  ) pacc = rcs cs p i pacc
    rn (JSStringLiteral (JSAnnot p cs) s l) pacc = rcs cs p ((s:l)++[s]) pacc
    rn (JSRegEx         (JSAnnot p cs) s  ) pacc = rcs cs p s pacc

    -- Non-Terminals
    rn (JSArguments            JSNoAnnot lb xs rb)                        pacc = rJS ([lb] ++ xs ++ [rb]) pacc
    rn (JSArrayLiteral         JSNoAnnot lb xs rb)                        pacc = rJS ([lb] ++ xs ++ [rb]) pacc
    rn (JSBlock                JSNoAnnot lb x rb)                         pacc = rJS (lb ++ x ++ rb) pacc
    rn (JSBreak                JSNoAnnot b x1s as)                        pacc = rJS ([b]++x1s++[as]) pacc
    rn (JSCallExpression       JSNoAnnot _s os xs cs)                     pacc = rJS (os ++ xs ++ cs) pacc
    rn (JSCase                 JSNoAnnot ca x1 c x2s)                     pacc = rJS ([ca,x1,c]++x2s) pacc
    rn (JSCatch                JSNoAnnot c lb x1 x2s rb x3)               pacc = rJS ([c,lb,x1]++x2s++[rb,x3]) pacc
    rn (JSContinue             JSNoAnnot c xs as)                         pacc = rJS ([c]++xs++[as]) pacc
    rn (JSDefault              JSNoAnnot d c xs)                          pacc = rJS ([d,c]++xs) pacc
    rn (JSDoWhile              JSNoAnnot d x1 w lb x2 rb x3)              pacc = rJS [d,x1,w,lb,x2,rb,x3] pacc
    rn (JSElision              JSNoAnnot c)                               pacc = rJS [c] pacc
    rn (JSExpression           JSNoAnnot xs)                              pacc = rJS xs pacc
    rn (JSExpressionBinary     JSNoAnnot lhs op rhs)                      pacc = rJS (lhs ++ [op] ++ rhs) pacc
    rn (JSExpressionParen      JSNoAnnot lb e rb)                         pacc = rJS [lb,e,rb] pacc
    rn (JSExpressionPostfix    JSNoAnnot xs op)                           pacc = rJS (xs ++ [op]) pacc
    rn (JSExpressionTernary    JSNoAnnot cond h v1 c v2)                  pacc = rJS (cond ++[h] ++ v1 ++ [c] ++ v2) pacc
    rn (JSFinally              JSNoAnnot f x)                             pacc = rJS [f,x] pacc
    rn (JSFor                  JSNoAnnot f lb x1s s1 x2s s2 x3s rb x4)    pacc = rJS ([f,lb]++x1s++[s1]++x2s++[s2]++x3s++[rb,x4]) pacc
    rn (JSForIn                JSNoAnnot f lb x1s i x2 rb x3)             pacc = rJS ([f,lb]++x1s++[i,x2,rb,x3]) pacc
    rn (JSForVar               JSNoAnnot f lb v x1s s1 x2s s2 x3s rb x4)  pacc = rJS ([f,lb,v]++x1s++[s1]++x2s++[s2]++x3s++[rb,x4]) pacc
    rn (JSForVarIn             JSNoAnnot f lb v x1 i x2 rb x3)            pacc = rJS [f,lb,v,x1,i,x2,rb,x3] pacc
    rn (JSFunction             JSNoAnnot f x1 lb x2s rb x3)               pacc = rJS ([f,x1,lb]++x2s++[rb,x3]) pacc
    rn (JSFunctionExpression   JSNoAnnot f x1s lb x2s rb x3)              pacc = rJS ([f] ++ x1s ++ [lb] ++ x2s ++ [rb,x3]) pacc
    rn (JSIf                   JSNoAnnot i lb x1 rb x2s x3s)              pacc = rJS ([i,lb,x1,rb]++x2s++x3s) pacc
    rn (JSLabelled             JSNoAnnot l c v)                           pacc = rJS [l,c,v] pacc
    rn (JSLiteral              JSNoAnnot [])                              pacc = rJS [] pacc
    rn (JSMemberDot            JSNoAnnot xs dot n)                        pacc = rJS (xs ++ [dot,n]) pacc
    rn (JSMemberSquare         JSNoAnnot xs lb e rb)                      pacc = rJS (xs ++ [lb,e,rb]) pacc
    rn (JSObjectLiteral        JSNoAnnot lb xs rb)                        pacc = rJS ([lb] ++ xs ++ [rb]) pacc
    rn (JSOperator             JSNoAnnot n)                               pacc = rJS [n] pacc
    rn (JSPropertyAccessor     JSNoAnnot s n lb1 ps rb1 b)                pacc = rJS ([s,n,lb1] ++ ps ++ [rb1,b]) pacc
    rn (JSPropertyNameandValue JSNoAnnot n colon vs)                      pacc = rJS ([n,colon] ++ vs) pacc
    rn (JSReturn               JSNoAnnot r xs as)                         pacc = rJS ([r] ++ xs ++ [as]) pacc
    rn (JSSourceElementsTop    JSNoAnnot xs)                              pacc = rJS xs pacc
    rn (JSSwitch               JSNoAnnot s lb x rb x2)                    pacc = rJS [s,lb,x,rb,x2] pacc
    rn (JSThrow                JSNoAnnot t x)                             pacc = rJS [t,x] pacc
    rn (JSTry                  JSNoAnnot t x1 x2s)                        pacc = rJS ([t,x1]++x2s) pacc
    rn (JSUnary                JSNoAnnot n)                               pacc = rJS [n] pacc
    rn (JSVarDecl              JSNoAnnot x1 x2s)                          pacc = rJS (x1:x2s) pacc
    rn (JSVariables            JSNoAnnot n xs as)                         pacc = rJS ([n]++xs++[as]) pacc
    rn (JSWhile                JSNoAnnot w lb x1 rb x2)                   pacc = rJS [w,lb,x1,rb,x2] pacc
    rn (JSWith                 JSNoAnnot w lb x1 rb x2s)                  pacc = rJS ([w,lb,x1,rb]++x2s) pacc

    -- Debug helper
    rn what pacc = rs ("X " ++ show what ++ " X") pacc

-- ---------------------------------------------------------------------
-- Helper functions

-- ---------------------------------------------------------------------
-- Need a function that
-- a) renders all comments, according to their positions
-- b) advances to the position of the required string
-- c) renders the string, advancing the position
rcs :: [CommentAnnotation] -> TokenPosn -> String -> PosAccum -> PosAccum
rcs cs p s pacc = rps p s (rc cs pacc)

rc :: [CommentAnnotation] -> PosAccum -> PosAccum
rc cs pacc = foldl' go pacc cs
  where
    go :: PosAccum -> CommentAnnotation -> PosAccum
    go pacc' NoComment = pacc'
    go pacc' (CommentA   p s) = rps p s pacc'
    go pacc' (WhiteSpace p s) = rps p s pacc'

-- Render a string at the given position
rps :: TokenPosn -> String -> PosAccum -> PosAccum
rps p s pacc = rs s pacc'
  where
    pacc' = goto p pacc

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
rJS xs pacc = foldl' (flip rn) pacc xs

renderToString :: JSNode -> String
-- need to be careful to not lose the unicode encoding on output
renderToString js = US.decode $ LB.unpack $ toLazyByteString $ renderJS js


-- EOF

