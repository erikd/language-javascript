{-# LANGUAGE FlexibleInstances #-}

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

str :: String -> Builder
str = BS.fromString

-- ---------------------------------------------------------------------

renderJS :: JSNode -> Builder
renderJS node = bb
  where
    PA _ bb = (PA (1,1) empty) |> node


renderToString :: JSNode -> String
-- need to be careful to not lose the unicode encoding on output
renderToString js = US.decode $ LB.unpack $ toLazyByteString $ renderJS js


class RenderJS a where
    -- Render node.
    (|>) :: PosAccum -> a -> PosAccum


instance RenderJS JSNode where
    -- Terminals
    (|>) pacc (JSIdentifier    annot s  ) = pacc |> annot |> s
    (|>) pacc (JSDecimal       annot i  ) = pacc |> annot |> i
    (|>) pacc (JSLiteral       annot l  ) = pacc |> annot |> l
    (|>) pacc (JSHexInteger    annot i  ) = pacc |> annot |> i
    (|>) pacc (JSOctal         annot i  ) = pacc |> annot |> i
    (|>) pacc (JSStringLiteral annot s l) = pacc |> annot |> ((s:l)++[s])
    (|>) pacc (JSRegEx         annot s  ) = pacc |> annot |> s

    -- Non-Terminals
    (|>) pacc (JSArguments            lb xs rb)                                  = pacc |> lb |> xs |> rb
    (|>) pacc (JSArrayLiteral         JSNoAnnot lb xs rb)                        = pacc |> lb |> xs |> rb
    (|>) pacc (JSBlock                JSNoAnnot lb x rb)                         = pacc |> lb |> x |> rb
    (|>) pacc (JSBreak                JSNoAnnot b x1s as)                        = pacc |> b |> x1s |> as
    (|>) pacc (JSCallExpression       JSNoAnnot _s os xs cs)                     = pacc |> os |> xs |> cs
    (|>) pacc (JSCallExpressionDot    JSNoAnnot _s os xs)                        = pacc |> os |> xs
    (|>) pacc (JSCallExpressionSquare JSNoAnnot _s os xs cs)                     = pacc |> os |> xs |> cs
    (|>) pacc (JSCase                 JSNoAnnot ca x1 c x2s)                     = pacc |> [ca,x1,c] |> x2s
    (|>) pacc (JSCatch                JSNoAnnot c lb x1 x2s rb x3)               = pacc |> c |> lb |> x1 |> x2s |> rb |> x3
    (|>) pacc (JSContinue             JSNoAnnot c xs as)                         = pacc |> c |> xs |> as
    (|>) pacc (JSDefault              JSNoAnnot d c xs)                          = pacc |> [d,c] |> xs
    (|>) pacc (JSDoWhile              JSNoAnnot d x1 w lb x2 rb x3)              = pacc |> [d, x1,w] |> lb |> x2 |> rb |> x3
    (|>) pacc (JSElision              JSNoAnnot c)                               = pacc |> c
    (|>) pacc (JSExpression           JSNoAnnot xs)                              = pacc |> xs
    (|>) pacc (JSExpressionBinary     JSNoAnnot lhs op rhs)                      = pacc |> lhs |> op |> rhs
    (|>) pacc (JSExpressionParen      JSNoAnnot lb e rb)                         = pacc |> lb |> e |> rb
    (|>) pacc (JSExpressionPostfix    JSNoAnnot xs op)                           = pacc |> xs |> op
    (|>) pacc (JSExpressionTernary    JSNoAnnot cond h v1 c v2)                  = pacc |> cond |> h |> v1 |> c |> v2
    (|>) pacc (JSFinally              JSNoAnnot f x)                             = pacc |> [f,x]
    (|>) pacc (JSFor                  JSNoAnnot f lb x1s s1 x2s s2 x3s rb x4)    = pacc |> f |> lb |> x1s |> s1 |> x2s |> s2 |> x3s |> rb |> x4
    (|>) pacc (JSForIn                JSNoAnnot f lb x1s i x2 rb x3)             = pacc |> f |> lb |> x1s |> i |> x2 |> rb |> x3
    (|>) pacc (JSForVar               JSNoAnnot f lb v x1s s1 x2s s2 x3s rb x4)  = pacc |> f |> lb |> v |> x1s |> s1 |> x2s |> s2 |> x3s |> rb |> x4
    (|>) pacc (JSForVarIn             JSNoAnnot f lb v x1 i x2 rb x3)            = pacc |> f |> lb |> [v,x1] |> i |> x2 |> rb |> x3
    (|>) pacc (JSFunction             JSNoAnnot f x1 lb x2s rb x3)               = pacc |> [f,x1] |> lb |> x2s |> rb |> x3
    (|>) pacc (JSFunctionExpression   JSNoAnnot f x1s lb x2s rb x3)              = pacc |> f |> x1s |> lb |> x2s |> rb |> x3
    (|>) pacc (JSIf                   JSNoAnnot i lb x1 rb x2s x3s)              = pacc |> i |> lb |> x1 |> rb |> x2s |> x3s
    (|>) pacc (JSLabelled             JSNoAnnot l c v)                           = pacc |> [l,c,v]
    (|>) pacc (JSMemberDot            JSNoAnnot xs dot n)                        = pacc |> xs |> [dot,n]
    (|>) pacc (JSMemberSquare         JSNoAnnot xs lb e rb)                      = pacc |> xs |> lb |> e |> rb
    (|>) pacc (JSObjectLiteral        JSNoAnnot lb xs rb)                        = pacc |> lb |> xs |> rb
    (|>) pacc (JSOperator             JSNoAnnot n)                               = pacc |> n
    (|>) pacc (JSPropertyAccessor     JSNoAnnot s n lb1 ps rb1 b)                = pacc |> [s,n] |> lb1 |> ps |> rb1 |> b
    (|>) pacc (JSPropertyNameandValue JSNoAnnot n colon vs)                      = pacc |> [n,colon] |> vs
    (|>) pacc (JSReturn               JSNoAnnot r xs as)                         = pacc |> r |> xs |> as
    (|>) pacc (JSSourceElementsTop    JSNoAnnot xs)                              = pacc |> xs
    (|>) pacc (JSSwitch               JSNoAnnot s lb x rb x2)                    = pacc |> s |> lb |> x |> rb |> x2
    (|>) pacc (JSThrow                JSNoAnnot t x)                             = pacc |> [t,x]
    (|>) pacc (JSTry                  JSNoAnnot t x1 x2s)                        = pacc |> t |> x1 |> x2s
    (|>) pacc (JSUnary                uop)                                       = pacc |> uop
    (|>) pacc (JSVarDecl              JSNoAnnot x1 x2s)                          = pacc |> x1 |> x2s
    (|>) pacc (JSVariables            JSNoAnnot n xs as)                         = pacc |> n |> xs |> as
    (|>) pacc (JSWhile                JSNoAnnot w lb x1 rb x2)                   = pacc |> w |> lb |> x1 |> rb |> x2
    (|>) pacc (JSWith                 JSNoAnnot w lb x1 rb x2s)                  = pacc |> w |> lb |> x1 |> rb |> x2s

    -- Debug helper
    (|>) pacc what = pacc |> ("X " ++ show what ++ " X")

-- -----------------------------------------------------------------------------
-- Need an instance of RenderJS for every component of every JSNode or JSAnnot
-- constuctor.
-- -----------------------------------------------------------------------------

instance RenderJS JSAnnot where
    (|>) pacc (JSAnnot p cs) = pacc |> cs |> p
    (|>) pacc JSNoAnnot = pacc


instance RenderJS String where
    (|>) (PA (r,c) bb) s = PA (r',c') (bb <> str s)
      where
        (r',c') = foldl' (\(row,col) ch -> go (row,col) ch) (r,c) s

        go (rx,_)  '\n' = (rx+1,1)
        go (rx,cx) '\t' = (rx,cx+8)
        go (rx,cx) _    = (rx,cx+1)


instance RenderJS TokenPosn where
    (|>)  (PA (lcur,ccur) bb) (TokenPn _ ltgt ctgt) = PA (lnew,cnew) (bb <> bb')
      where
        (bbline,ccur') = if lcur < ltgt then (str (replicate (ltgt - lcur) '\n'),1) else (mempty,ccur)
        bbcol  = if ccur' < ctgt then str (replicate (ctgt - ccur') ' ') else mempty
        bb' = bbline <> bbcol
        lnew = if lcur < ltgt then ltgt else lcur
        cnew = if ccur' < ctgt then ctgt else ccur'


instance RenderJS [CommentAnnotation] where
    (|>) pacc cs = foldl' (|>) pacc cs


instance RenderJS CommentAnnotation where
    (|>) pacc NoComment = pacc
    (|>) pacc (CommentA   p s) = pacc |> p |> s
    (|>) pacc (WhiteSpace p s) = pacc |> p |> s


instance RenderJS [JSNode] where
    (|>) pacc xs = foldl' (|>) pacc xs


instance RenderJS JSBinOp where
    (|>) pacc (JSBinOpAnd        annot)  = pacc |> annot |> "&&"
    (|>) pacc (JSBinOpBitAnd     annot)  = pacc |> annot |> "&"
    (|>) pacc (JSBinOpBitOr      annot)  = pacc |> annot |> "|"
    (|>) pacc (JSBinOpBitXor     annot)  = pacc |> annot |> "^"
    (|>) pacc (JSBinOpDivide     annot)  = pacc |> annot |> "/"
    (|>) pacc (JSBinOpEq         annot)  = pacc |> annot |> "=="
    (|>) pacc (JSBinOpGe         annot)  = pacc |> annot |> ">="
    (|>) pacc (JSBinOpGt         annot)  = pacc |> annot |> ">"
    (|>) pacc (JSBinOpIn         annot)  = pacc |> annot |> "in"
    (|>) pacc (JSBinOpInstanceOf annot)  = pacc |> annot |> "instanceof"
    (|>) pacc (JSBinOpLe         annot)  = pacc |> annot |> "<="
    (|>) pacc (JSBinOpLsh        annot)  = pacc |> annot |> "<<"
    (|>) pacc (JSBinOpLt         annot)  = pacc |> annot |> "<"
    (|>) pacc (JSBinOpMinus      annot)  = pacc |> annot |> "-"
    (|>) pacc (JSBinOpMod        annot)  = pacc |> annot |> "%"
    (|>) pacc (JSBinOpNeq        annot)  = pacc |> annot |> "!="
    (|>) pacc (JSBinOpOr         annot)  = pacc |> annot |> "||"
    (|>) pacc (JSBinOpPlus       annot)  = pacc |> annot |> "+"
    (|>) pacc (JSBinOpRsh        annot)  = pacc |> annot |> ">>"
    (|>) pacc (JSBinOpStrictEq   annot)  = pacc |> annot |> "==="
    (|>) pacc (JSBinOpStrictNeq  annot)  = pacc |> annot |> "!=="
    (|>) pacc (JSBinOpTimes      annot)  = pacc |> annot |> "*"
    (|>) pacc (JSBinOpUrsh       annot)  = pacc |> annot |> ">>>"


instance RenderJS JSUnaryOp where
    (|>) pacc (JSUnaryOpDecr   annot) = pacc |> annot |> "--"
    (|>) pacc (JSUnaryOpDelete annot) = pacc |> annot |> "delete"
    (|>) pacc (JSUnaryOpIncr   annot) = pacc |> annot |> "++"
    (|>) pacc (JSUnaryOpMinus  annot) = pacc |> annot |> "-"
    (|>) pacc (JSUnaryOpNot    annot) = pacc |> annot |> "!"
    (|>) pacc (JSUnaryOpPlus   annot) = pacc |> annot |> "+"
    (|>) pacc (JSUnaryOpTilde  annot) = pacc |> annot |> "~"
    (|>) pacc (JSUnaryOpTypeof annot) = pacc |> annot |> "typeof"
    (|>) pacc (JSUnaryOpVoid   annot) = pacc |> annot |> "void"


instance RenderJS JSLParen where
    (|>) pacc (JSLParen annot) = pacc |> annot |> "("

instance RenderJS JSRParen where
    (|>) pacc (JSRParen annot) = pacc |> annot |> ")"

instance RenderJS JSLBrace where
    (|>) pacc (JSLBrace annot) = pacc |> annot |> "{"

instance RenderJS JSRBrace where
    (|>) pacc (JSRBrace annot) = pacc |> annot |> "}"

instance RenderJS JSLSquare where
    (|>) pacc (JSLSquare annot) = pacc |> annot |> "["

instance RenderJS JSRSquare where
    (|>) pacc (JSRSquare annot) = pacc |> annot |> "]"


-- TODO: The following should not be necessary.
instance RenderJS [JSLBrace] where
    (|>) pacc [] = pacc
    (|>) pacc [JSLBrace annot] = pacc |> annot |> "{"
    (|>) _ _ = error "RenderJS [JSLBrace]"

instance RenderJS [JSRBrace] where
    (|>) pacc [] = pacc
    (|>) pacc [JSRBrace annot] = pacc |> annot |> "}"
    (|>) _ _ = error "RenderJS [JSRBrace]"

instance RenderJS [JSLSquare] where
    (|>) pacc [] = pacc
    (|>) pacc [JSLSquare annot] = pacc |> annot |> "["
    (|>) _ _ = error "RenderJS [JSLSquare]"

instance RenderJS [JSRSquare] where
    (|>) pacc [] = pacc
    (|>) pacc [JSRSquare annot] = pacc |> annot |> "]"
    (|>) _ _ = error "RenderJS [JSRSquare]"

-- EOF

