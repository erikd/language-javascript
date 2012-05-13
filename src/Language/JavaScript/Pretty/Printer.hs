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
    PA _ bb = PA (1,1) empty |> node


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
    (|>) pacc (JSArrayLiteral         lb xs rb)                                  = pacc |> lb |> xs |> rb
    (|>) pacc (JSAssignExpression     lhs op rhs)                                = pacc |> lhs |> op |> rhs
    (|>) pacc (JSBlock                lb xs rb)                                  = pacc |> lb |> xs |> rb
    (|>) pacc (JSBreak                annot x1s s)                               = pacc |> annot |> "break" |> x1s |> s
    (|>) pacc (JSCallExpression       os xs cs)                                  = pacc |> os |> xs |> cs
    (|>) pacc (JSCallExpressionDot    os xs)                                     = pacc |> os |> xs
    (|>) pacc (JSCallExpressionSquare os xs cs)                                  = pacc |> os |> xs |> cs
    (|>) pacc (JSCase                 annot x1 c x2s)                            = pacc |> annot |> "case" |> x1 |> c |> x2s
    (|>) pacc (JSConstant             annot xs s)                                = pacc |> annot |> "const" |> xs |> s
    (|>) pacc (JSContinue             annot xs s)                                = pacc |> annot |> "continue" |> xs |> s
    (|>) pacc (JSDefault              annot c xs)                                = pacc |> annot |> "default" |> c |> xs
    (|>) pacc (JSDoWhile              annot x1 annotw lb x2 rb x3)               = pacc |> annot |> "do" |> x1 |> annotw |> "while" |> lb |> x2 |> rb |> x3
    (|>) pacc (JSElision              JSNoAnnot c)                               = pacc |> c
    (|>) pacc (JSExpression           xs)                                        = pacc |> xs
    (|>) pacc (JSExpressionBinary     lhs op rhs)                                = pacc |> lhs |> op |> rhs
    (|>) pacc (JSExpressionParen      lb e rb)                                   = pacc |> lb |> e |> rb
    (|>) pacc (JSExpressionPostfix    xs op)                                     = pacc |> xs |> op
    (|>) pacc (JSExpressionTernary    cond h v1 c v2)                            = pacc |> cond |> h |> v1 |> c |> v2
    (|>) pacc (JSFor                  JSNoAnnot f lb x1s s1 x2s s2 x3s rb x4)    = pacc |> f |> lb |> x1s |> s1 |> x2s |> s2 |> x3s |> rb |> x4
    (|>) pacc (JSForIn                JSNoAnnot f lb x1s i x2 rb x3)             = pacc |> f |> lb |> x1s |> i |> x2 |> rb |> x3
    (|>) pacc (JSForVar               JSNoAnnot f lb v x1s s1 x2s s2 x3s rb x4)  = pacc |> f |> lb |> v |> x1s |> s1 |> x2s |> s2 |> x3s |> rb |> x4
    (|>) pacc (JSForVarIn             JSNoAnnot f lb v x1 i x2 rb x3)            = pacc |> f |> lb |> v |> x1 |> i |> x2 |> rb |> x3
    (|>) pacc (JSFunction             f x1 lb x2s rb x3)                         = pacc |> f |> x1 |> lb |> x2s |> rb |> x3
    (|>) pacc (JSFunctionExpression   f x1s lb x2s rb x3)                        = pacc |> f |> x1s |> lb |> x2s |> rb |> x3
    (|>) pacc (JSIf                   JSNoAnnot i lb x1 rb x2s x3s)              = pacc |> i |> lb |> x1 |> rb |> x2s |> x3s
    (|>) pacc (JSLabelled             JSNoAnnot l c v)                           = pacc |> l |> c |> v
    (|>) pacc (JSMemberDot            xs dot n)                                  = pacc |> xs |> dot |> n
    (|>) pacc (JSMemberSquare         xs lb e rb)                                = pacc |> xs |> lb |> e |> rb
    (|>) pacc (JSObjectLiteral        lb xs rb)                                  = pacc |> lb |> xs |> rb
    (|>) pacc (JSOpAssign              n)                                        = pacc |> n
    (|>) pacc (JSPropertyAccessor     JSNoAnnot s n lb1 ps rb1 b)                = pacc |> s |> n |> lb1 |> ps |> rb1 |> b
    (|>) pacc (JSPropertyNameandValue JSNoAnnot n colon vs)                      = pacc |> n |> colon |> vs
    (|>) pacc (JSReturn               annot xs s)                                = pacc |> annot |> "return" |> xs |> s
    (|>) pacc (JSSourceElementsTop    JSNoAnnot xs)                              = pacc |> xs
    (|>) pacc (JSSwitch               annot lb x rb x2)                          = pacc |> annot |> "switch" |> lb |> x |> rb |> x2
    (|>) pacc (JSThrow                annot x)                                   = pacc |> annot |> "throw" |> x
    (|>) pacc (JSTry                  annot tb tcs tf)                           = pacc |> annot |> "try" |> tb |> tcs |> tf
    (|>) pacc (JSUnaryExpression      op x)                                      = pacc |> op |> x
    (|>) pacc (JSVarDecl              x1 x2s)                                    = pacc |> x1 |> x2s
    (|>) pacc (JSVariable             annot xs a)                                = pacc |> annot |> "var" |> xs |> a
    (|>) pacc (JSWhile                annot lb x1 rb x2)                         = pacc |> annot |> "while" |> lb |> x1 |> rb |> x2
    (|>) pacc (JSWith                 annot lb x1 rb x s)                        = pacc |> annot |> "with" |> lb |> x1 |> rb |> x |> s

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
    (|>) = foldl' (|>)


instance RenderJS CommentAnnotation where
    (|>) pacc NoComment = pacc
    (|>) pacc (CommentA   p s) = pacc |> p |> s
    (|>) pacc (WhiteSpace p s) = pacc |> p |> s


instance RenderJS [JSNode] where
    (|>) = foldl' (|>)


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


instance RenderJS JSAssignOp where
    (|>) pacc (JSAssign       annot) = pacc |> annot |> "="
    (|>) pacc (JSTimesAssign  annot) = pacc |> annot |> "*="
    (|>) pacc (JSDivideAssign annot) = pacc |> annot |> "/="
    (|>) pacc (JSModAssign    annot) = pacc |> annot |> "%="
    (|>) pacc (JSPlusAssign   annot) = pacc |> annot |> "+="
    (|>) pacc (JSMinusAssign  annot) = pacc |> annot |> "-="
    (|>) pacc (JSLshAssign    annot) = pacc |> annot |> "<<="
    (|>) pacc (JSRshAssign    annot) = pacc |> annot |> ">>="
    (|>) pacc (JSUrshAssign   annot) = pacc |> annot |> ">>>="
    (|>) pacc (JSBwAndAssign  annot) = pacc |> annot |> "&="
    (|>) pacc (JSBwXorAssign  annot) = pacc |> annot |> "^="
    (|>) pacc (JSBwOrAssign   annot) = pacc |> annot |> "|="


instance RenderJS JSTryCatch where
    (|>) pacc (JSCatch        annot lb x1 x2s rb x3) = pacc |> annot |> "catch" |> lb |> x1 |> x2s |> rb |> x3

instance RenderJS [JSTryCatch] where
    (|>) = foldl' (|>)

instance RenderJS JSTryFinally where
    (|>) pacc (JSFinally      annot x) = pacc |> annot |> "finally" |> x
    (|>) pacc JSNoFinally              = pacc


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

instance RenderJS JSSemi where
    (|>) pacc (JSSemi annot) = pacc |> annot |> ";"
    (|>) pacc JSSemiAuto     = pacc

-- EOF

