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

renderJS :: JSAST -> Builder
renderJS node = bb
  where
    PA _ bb = PA (1,1) empty |> node


renderToString :: JSAST -> String
-- need to be careful to not lose the unicode encoding on output
renderToString js = US.decode $ LB.unpack $ toLazyByteString $ renderJS js


class RenderJS a where
    -- Render node.
    (|>) :: PosAccum -> a -> PosAccum


instance RenderJS JSAST where
    (|>) pacc (JSSourceElementsTop xs) = pacc |> xs


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
    (|>) pacc (JSArguments            alp xs arp)             = pacc |> alp |> "(" |> xs |> arp |> ")"
    (|>) pacc (JSArrayLiteral         als xs ars)             = pacc |> als |> "[" |> xs |> ars |> "]"
    (|>) pacc (JSAssignExpression     lhs op rhs)             = pacc |> lhs |> op |> rhs
    (|>) pacc (JSCallExpression       os xs cs)               = pacc |> os |> xs |> cs
    (|>) pacc (JSCallExpressionDot    os xs)                  = pacc |> os |> xs
    (|>) pacc (JSCallExpressionSquare als xs ars)             = pacc |> als |> "[" |> xs |> ars |> "]"
    (|>) pacc (JSElision              c)                      = pacc |> c
    (|>) pacc (JSExpression           xs)                     = pacc |> xs
    (|>) pacc (JSExpressionBinary     lhs op rhs)             = pacc |> lhs |> op |> rhs
    (|>) pacc (JSExpressionParen      alp e arp)              = pacc |> alp |> "(" |> e |> arp |> ")"
    (|>) pacc (JSExpressionPostfix    xs op)                  = pacc |> xs |> op
    (|>) pacc (JSExpressionTernary    cond h v1 c v2)         = pacc |> cond |> h |> v1 |> c |> v2
    (|>) pacc (JSFunctionExpression   annot x1s lb x2s rb x3) = pacc |> annot |> "function" |> x1s |> lb |> "(" |> x2s |> rb |> ")" |> x3
    (|>) pacc (JSMemberDot            xs dot n)               = pacc |> xs |> dot |> n
    (|>) pacc (JSMemberSquare         xs als e ars)           = pacc |> xs |> als |> "[" |> e |> ars |> "]"
    (|>) pacc (JSObjectLiteral        alb xs arb)             = pacc |> alb |> "{" |> xs |> arb |> "}"
    (|>) pacc (JSOpAssign             n)                      = pacc |> n
    (|>) pacc (JSPropertyAccessor     s n alp ps arp b)       = pacc |> s |> n |> alp |> "(" |> ps |> arp |> ")" |> b
    (|>) pacc (JSPropertyNameandValue n colon vs)             = pacc |> n |> colon |> vs
    (|>) pacc (JSUnaryExpression      op x)                   = pacc |> op |> x

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


instance RenderJS JSSemi where
    (|>) pacc (JSSemi annot) = pacc |> annot |> ";"
    (|>) pacc JSSemiAuto     = pacc


instance RenderJS JSTryCatch where
    (|>) pacc (JSCatch anc alb x1 x2s arb x3) = pacc |> anc |> "catch" |> alb |> "(" |> x1 |> x2s |> arb |> ")" |> x3

instance RenderJS [JSTryCatch] where
    (|>) = foldl' (|>)

instance RenderJS JSTryFinally where
    (|>) pacc (JSFinally      annot x) = pacc |> annot |> "finally" |> x
    (|>) pacc JSNoFinally              = pacc

instance RenderJS JSSwitchParts where
    (|>) pacc (JSCase    annot x1 c x2s) = pacc |> annot |> "case" |> x1 |> c |> x2s
    (|>) pacc (JSDefault annot c xs)     = pacc |> annot |> "default" |> c |> xs

instance RenderJS [JSSwitchParts] where
    (|>) = foldl' (|>)

instance RenderJS JSStatement where
    (|>) pacc (JSStatementBlock blk)                       = pacc |> blk
    (|>) pacc (JSBreak annot x1s s)                        = pacc |> annot |> "break" |> x1s |> s
    (|>) pacc (JSContinue annot xs s)                      = pacc |> annot |> "continue" |> xs |> s
    (|>) pacc (JSConstant annot xs s)                      = pacc |> annot |> "const" |> xs |> s
    (|>) pacc (JSDoWhile ad x1 aw alb x2 arb x3)           = pacc |> ad |> "do" |> x1 |> aw |> "while" |> alb |> "(" |> x2 |> arb |> ")" |> x3
    (|>) pacc (JSFor af alb x1s s1 x2s s2 x3s arb x4)      = pacc |> af |> "for" |> alb |> "(" |> x1s |> s1 |> x2s |> s2 |> x3s |> arb |> ")" |> x4
    (|>) pacc (JSForIn af alb x1s i x2 arb x3)             = pacc |> af |> "for" |> alb |> "(" |> x1s |> i |> x2 |> arb |> ")" |> x3
    (|>) pacc (JSForVar af alb v x1s s1 x2s s2 x3s arb x4) = pacc |> af |> "for" |> alb |> "(" |> v |> x1s |> s1 |> x2s |> s2 |> x3s |> arb |> ")" |> x4
    (|>) pacc (JSForVarIn af alb v x1 i x2 arb x3)         = pacc |> af |> "for" |> alb |> "(" |> v |> x1 |> i |> x2 |> arb |> ")" |> x3
    (|>) pacc (JSFunction af n alb x2s arb x3)             = pacc |> af |> "function" |> n |> alb |> "(" |> x2s |> arb |> ")" |> x3
    (|>) pacc (JSIf annot alb x1 arb x2s x3s)              = pacc |> annot |> "if" |> alb |> "(" |> x1 |> arb |> ")" |> x2s |> x3s
    (|>) pacc (JSLabelled l c v)                           = pacc |> l |> c |> v
    (|>) pacc (JSExpressionStatement l)                    = pacc |> l
    (|>) pacc (JSReturn annot xs s)                        = pacc |> annot |> "return" |> xs |> s
    (|>) pacc (JSSwitch annot alp x arp alb x2 arb)        = pacc |> annot |> "switch" |> alp |> "(" |> x |> arp |> ")" |> alb |> "{" |> x2 |> arb |> "}"
    (|>) pacc (JSThrow annot x)                            = pacc |> annot |> "throw" |> x
    (|>) pacc (JSTry annot tb tcs tf)                      = pacc |> annot |> "try" |> tb |> tcs |> tf
    (|>) pacc (JSVarDecl x1 x2s)                           = pacc |> x1 |> x2s
    (|>) pacc (JSVariable annot xs s)                      = pacc |> annot |> "var" |> xs |> s
    (|>) pacc (JSWhile annot alp x1 arp x2)                = pacc |> annot |> "while" |> alp |> "(" |> x1 |> arp |> ")" |> x2
    (|>) pacc (JSWith annot alp x1 arp x s)                = pacc |> annot |> "with" |> alp |> "(" |> x1 |> arp |> ")" |> x |> s


instance RenderJS [JSStatement] where
    (|>) = foldl' (|>)

instance RenderJS JSBlock where
    (|>) pacc (JSBlock alb ss arb) = pacc |> alb |> "{" |> ss |> arb |> "}"

-- EOF

