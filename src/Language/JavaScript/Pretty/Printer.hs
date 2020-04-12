{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoOverloadedStrings  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.JavaScript.Pretty.Printer
    ( -- * Printing
      renderJS
    , renderToString
    , renderToText
    ) where

import           Blaze.ByteString.Builder               (Builder,
                                                         toLazyByteString)
import           Data.List
#if ! MIN_VERSION_base(4,13,0)
import           Data.Monoid                            (mempty)
import           Data.Semigroup                         ((<>))
#endif
import qualified Blaze.ByteString.Builder.Char.Utf8     as BS
import qualified Codec.Binary.UTF8.String               as US
import qualified Data.ByteString.Lazy                   as LB
import           Data.Text.Lazy                         (Text)
import qualified Data.Text.Lazy.Encoding                as LT
import           Language.JavaScript.Parser.AST
import           Language.JavaScript.Parser.SrcLocation
import           Language.JavaScript.Parser.Token

-- ---------------------------------------------------------------------

data PosAccum = PosAccum (Int, Int) Builder

-- ---------------------------------------------------------------------
-- Pretty printer stuff via blaze-builder

str :: String -> Builder
str = BS.fromString

-- ---------------------------------------------------------------------

renderJS :: JSAST -> Builder
renderJS node = bb
  where
    PosAccum _ bb = PosAccum (1,1) mempty |> node


renderToString :: JSAST -> String
-- need to be careful to not lose the unicode encoding on output
renderToString js = US.decode $ LB.unpack $ toLazyByteString $ renderJS js

renderToText :: JSAST -> Text
-- need to be careful to not lose the unicode encoding on output
renderToText = LT.decodeUtf8 . toLazyByteString . renderJS


class RenderJS a where
    -- Render node.
    (|>) :: PosAccum -> a -> PosAccum


instance RenderJS JSAST where
    (|>) pacc (JSAstProgram xs a)   = pacc |> xs |> a
    (|>) pacc (JSAstModule xs a)    = pacc |> xs |> a
    (|>) pacc (JSAstStatement s a)  = pacc |> s |> a
    (|>) pacc (JSAstExpression e a) = pacc |> e |> a
    (|>) pacc (JSAstLiteral x a)    = pacc |> x |> a

instance RenderJS JSExpression where
    -- Terminals
    (|>) pacc (JSIdentifier     annot s) = pacc |> annot |> s
    (|>) pacc (JSDecimal        annot i) = pacc |> annot |> i
    (|>) pacc (JSLiteral        annot l) = pacc |> annot |> l
    (|>) pacc (JSHexInteger     annot i) = pacc |> annot |> i
    (|>) pacc (JSOctal          annot i) = pacc |> annot |> i
    (|>) pacc (JSStringLiteral  annot s) = pacc |> annot |> s
    (|>) pacc (JSRegEx          annot s) = pacc |> annot |> s

    -- Non-Terminals
    (|>) pacc (JSArrayLiteral         als xs ars)             = pacc |> als |> "[" |> xs |> ars |> "]"
    (|>) pacc (JSArrowExpression      xs a x)                 = pacc |> xs |> a |> "=>" |> x
    (|>) pacc (JSCallExpression       ex lb xs rb)            = pacc |> ex |> lb |> "(" |> xs |> rb |> ")"
    (|>) pacc (JSCallExpressionDot    ex os xs)               = pacc |> ex |> os |> "." |> xs
    (|>) pacc (JSCallExpressionSquare ex als xs ars)          = pacc |> ex |> als |> "[" |> xs |> ars |> "]"
    (|>) pacc (JSClassExpression      annot n h lb xs rb)     = pacc |> annot |> "class" |> n |> h |> lb |> "{" |> xs |> rb |> "}"
    (|>) pacc (JSCommaExpression      le c re)                = pacc |> le |> c |> "," |> re
    (|>) pacc (JSExpressionBinary     lhs op rhs)             = pacc |> lhs |> op |> rhs
    (|>) pacc (JSExpressionParen      alp e arp)              = pacc |> alp |> "(" |> e |> arp |> ")"
    (|>) pacc (JSExpressionPostfix    xs op)                  = pacc |> xs |> op
    (|>) pacc (JSExpressionTernary    cond h v1 c v2)         = pacc |> cond |> h |> "?" |> v1 |> c |> ":" |> v2
    (|>) pacc (JSFunctionExpression   annot n lb x2s rb x3)   = pacc |> annot |> "function" |> n |> lb |> "(" |> x2s |> rb |> ")" |> x3
    (|>) pacc (JSMemberDot            xs dot n)               = pacc |> xs |> "." |> dot |> n
    (|>) pacc (JSMemberExpression     e lb a rb)              = pacc |> e |> lb |> "(" |> a |> rb |> ")"
    (|>) pacc (JSMemberNew            a lb n rb s)            = pacc |> a |> "new" |> lb |> "(" |> n |> rb |> ")" |> s
    (|>) pacc (JSMemberSquare         xs als e ars)           = pacc |> xs |> als |> "[" |> e |> ars |> "]"
    (|>) pacc (JSObjectLiteral        alb xs arb)             = pacc |> alb |> "{" |> xs |> arb |> "}"
    (|>) pacc (JSTemplateLiteral      t a h ps)               = pacc |> t |> a |> h |> ps
    (|>) pacc (JSUnaryExpression      op x)                   = pacc |> op |> x
    (|>) pacc (JSVarInitExpression    x1 x2)                  = pacc |> x1 |> x2
    (|>) pacc (JSSpreadExpression     a e)                    = pacc |> a |> "..." |> e

instance RenderJS JSArrowParameterList where
    (|>) pacc (JSUnparenthesizedArrowParameter p)             = pacc |> p
    (|>) pacc (JSParenthesizedArrowParameterList lb ps rb)    = pacc |> lb |> "(" |> ps |> ")" |> rb
-- -----------------------------------------------------------------------------
-- Need an instance of RenderJS for every component of every JSExpression or JSAnnot
-- constuctor.
-- -----------------------------------------------------------------------------

instance RenderJS JSAnnot where
    (|>) pacc (JSAnnot p cs) = pacc |> cs |> p
    (|>) pacc JSNoAnnot      = pacc
    (|>) pacc JSAnnotSpace   = pacc |> " "

instance RenderJS String where
    (|>) (PosAccum (r,c) bb) s = PosAccum (r',c') (bb <> str s)
      where
        (r',c') = foldl' (\(row,col) ch -> go (row,col) ch) (r,c) s

        go (rx,_)  '\n' = (rx+1,1)
        go (rx,cx) '\t' = (rx,cx+8)
        go (rx,cx) _    = (rx,cx+1)


instance RenderJS TokenPosn where
    (|>)  (PosAccum (lcur,ccur) bb) (TokenPn _ ltgt ctgt) = PosAccum (lnew,cnew) (bb <> bb')
      where
        (bbline,ccur') = if lcur < ltgt then (str (replicate (ltgt - lcur) '\n'),1) else (mempty,ccur)
        bbcol  = if ccur' < ctgt then str (replicate (ctgt - ccur') ' ') else mempty
        bb' = bbline <> bbcol
        lnew = if lcur < ltgt then ltgt else lcur
        cnew = if ccur' < ctgt then ctgt else ccur'


instance RenderJS [CommentAnnotation] where
    (|>) = foldl' (|>)


instance RenderJS CommentAnnotation where
    (|>) pacc NoComment        = pacc
    (|>) pacc (CommentA   p s) = pacc |> p |> s
    (|>) pacc (WhiteSpace p s) = pacc |> p |> s


instance RenderJS [JSExpression] where
    (|>) = foldl' (|>)


instance RenderJS JSBinOp where
    (|>) pacc (JSBinOpAnd        annot) = pacc |> annot |> "&&"
    (|>) pacc (JSBinOpBitAnd     annot) = pacc |> annot |> "&"
    (|>) pacc (JSBinOpBitOr      annot) = pacc |> annot |> "|"
    (|>) pacc (JSBinOpBitXor     annot) = pacc |> annot |> "^"
    (|>) pacc (JSBinOpDivide     annot) = pacc |> annot |> "/"
    (|>) pacc (JSBinOpEq         annot) = pacc |> annot |> "=="
    (|>) pacc (JSBinOpGe         annot) = pacc |> annot |> ">="
    (|>) pacc (JSBinOpGt         annot) = pacc |> annot |> ">"
    (|>) pacc (JSBinOpIn         annot) = pacc |> annot |> "in"
    (|>) pacc (JSBinOpInstanceOf annot) = pacc |> annot |> "instanceof"
    (|>) pacc (JSBinOpLe         annot) = pacc |> annot |> "<="
    (|>) pacc (JSBinOpLsh        annot) = pacc |> annot |> "<<"
    (|>) pacc (JSBinOpLt         annot) = pacc |> annot |> "<"
    (|>) pacc (JSBinOpMinus      annot) = pacc |> annot |> "-"
    (|>) pacc (JSBinOpMod        annot) = pacc |> annot |> "%"
    (|>) pacc (JSBinOpNeq        annot) = pacc |> annot |> "!="
    (|>) pacc (JSBinOpOf         annot) = pacc |> annot |> "of"
    (|>) pacc (JSBinOpOr         annot) = pacc |> annot |> "||"
    (|>) pacc (JSBinOpPlus       annot) = pacc |> annot |> "+"
    (|>) pacc (JSBinOpRsh        annot) = pacc |> annot |> ">>"
    (|>) pacc (JSBinOpStrictEq   annot) = pacc |> annot |> "==="
    (|>) pacc (JSBinOpStrictNeq  annot) = pacc |> annot |> "!=="
    (|>) pacc (JSBinOpTimes      annot) = pacc |> annot |> "*"
    (|>) pacc (JSBinOpUrsh       annot) = pacc |> annot |> ">>>"


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
    (|>) pacc (JSCatch anc alb x1 arb x3) = pacc |> anc |> "catch" |> alb |> "(" |> x1 |> arb |> ")" |> x3
    (|>) pacc (JSCatchIf anc alb x1 aif ex arb x3) = pacc |> anc |> "catch" |> alb |> "(" |> x1 |> aif |> "if" |> ex |> arb |> ")" |> x3

instance RenderJS [JSTryCatch] where
    (|>) = foldl' (|>)

instance RenderJS JSTryFinally where
    (|>) pacc (JSFinally      annot x) = pacc |> annot |> "finally" |> x
    (|>) pacc JSNoFinally              = pacc

instance RenderJS JSSwitchParts where
    (|>) pacc (JSCase    annot x1 c x2s) = pacc |> annot |> "case" |> x1 |> c |> ":" |> x2s
    (|>) pacc (JSDefault annot c xs)     = pacc |> annot |> "default" |> c |> ":" |> xs

instance RenderJS [JSSwitchParts] where
    (|>) = foldl' (|>)

instance RenderJS JSStatement where
    (|>) pacc (JSStatementBlock alb blk arb s)             = pacc |> alb |> "{" |> blk |> arb |> "}" |> s
    (|>) pacc (JSBreak annot mi s)                         = pacc |> annot |> "break" |> mi |> s
    (|>) pacc (JSConstant annot xs s)                      = pacc |> annot |> "const" |> xs |> s
    (|>) pacc (JSEmptyStatement a)                         = pacc |> a |> ";"
    (|>) pacc (JSFunction af n alb x2s arb x3 s)           = pacc |> af |> "function" |> n |> alb |> "(" |> x2s |> arb |> ")" |> x3 |> s
    (|>) pacc (JSIf annot alb x1 arb x2s)                  = pacc |> annot |> "if" |> alb |> "(" |> x1 |> arb |> ")" |> x2s
    (|>) pacc (JSIfElse annot alb x1 arb x2s ea x3s)       = pacc |> annot |> "if" |> alb |> "(" |> x1 |> arb |> ")" |> x2s |> ea |> "else" |> x3s
    (|>) pacc (JSLabelled l c v)                           = pacc |> l |> c |> ":" |> v
    (|>) pacc (JSExpressionStatement l s)                  = pacc |> l |> s
    (|>) pacc (JSMethodCall e lp a rp s)                   = pacc |> e |> lp |> "(" |> a |> rp |> ")" |> s
    (|>) pacc (JSReturn annot me s)                        = pacc |> annot |> "return" |> me |> s
    (|>) pacc (JSSwitch annot alp x arp alb x2 arb s)      = pacc |> annot |> "switch" |> alp |> "(" |> x |> arp |> ")" |> alb |> "{" |> x2 |> arb |> "}" |> s
    (|>) pacc (JSThrow annot x s)                          = pacc |> annot |> "throw" |> x |> s

instance RenderJS [JSStatement] where
    (|>) = foldl' (|>)

instance RenderJS [JSModuleItem] where
    (|>) = foldl' (|>)

instance RenderJS JSModuleItem where
    (|>) pacc (JSModuleImportDeclaration annot decl) = pacc |> annot |> "import" |> decl
    (|>) pacc (JSModuleExportDeclaration annot decl) = pacc |> annot |> "export" |> decl
    (|>) pacc (JSModuleStatementListItem s) = pacc |> s

instance RenderJS JSBlock where
    (|>) pacc (JSBlock alb ss arb) = pacc |> alb |> "{" |> ss |> arb |> "}"

instance RenderJS JSObjectProperty where
    (|>) pacc (JSPropertyNameandValue n c vs) = pacc |> n |> c |> ":" |> vs
    (|>) pacc (JSPropertyIdentRef     a s)    = pacc |> a |> s
    (|>) pacc (JSObjectMethod         m)      = pacc |> m

instance RenderJS JSMethodDefinition where
    (|>) pacc (JSMethodDefinition          n alp ps arp b)   = pacc |> n |> alp |> "(" |> ps |> arp |> ")" |> b
    (|>) pacc (JSGeneratorMethodDefinition s n alp ps arp b) = pacc |> s |> "*" |> n |> alp |> "(" |> ps |> arp |> ")" |> b
    (|>) pacc (JSPropertyAccessor          s n alp ps arp b) = pacc |> s |> n |> alp |> "(" |> ps |> arp |> ")" |> b

instance RenderJS JSPropertyName where
    (|>) pacc (JSPropertyIdent a s)  = pacc |> a |> s
    (|>) pacc (JSPropertyString a s) = pacc |> a |> s
    (|>) pacc (JSPropertyNumber a s) = pacc |> a |> s
    (|>) pacc (JSPropertyComputed lb x rb) = pacc |> lb |> "[" |> x |> rb |> "]"

instance RenderJS JSAccessor where
    (|>) pacc (JSAccessorGet annot) = pacc |> annot |> "get"
    (|>) pacc (JSAccessorSet annot) = pacc |> annot |> "set"

instance RenderJS JSArrayElement where
    (|>) pacc (JSArrayElement e) = pacc |> e
    (|>) pacc (JSArrayComma a)   = pacc |> a |> ","

instance RenderJS [JSArrayElement] where
    (|>) = foldl' (|>)

instance RenderJS JSImportDeclaration where
    (|>) pacc (JSImportDeclaration imp from annot) = pacc |> imp |> from |> annot
    (|>) pacc (JSImportDeclarationBare annot m s) = pacc |> annot |> m |> s

instance RenderJS JSImportClause where
    (|>) pacc (JSImportClauseDefault x) = pacc |> x
    (|>) pacc (JSImportClauseNameSpace x) = pacc |> x
    (|>) pacc (JSImportClauseNamed x) = pacc |> x
    (|>) pacc (JSImportClauseDefaultNameSpace x1 annot x2) = pacc |> x1 |> annot |> "," |> x2
    (|>) pacc (JSImportClauseDefaultNamed x1 annot x2) = pacc |> x1 |> annot |> "," |> x2

instance RenderJS JSFromClause where
    (|>) pacc (JSFromClause from annot m) = pacc |> from |> "from" |> annot |> m

instance RenderJS JSImportNameSpace where
    (|>) pacc (JSImportNameSpace star annot x) = pacc |> star |> annot |> "as" |> x

instance RenderJS JSImportsNamed where
    (|>) pacc (JSImportsNamed lb xs rb) = pacc |> lb |> "{" |> xs |> rb |> "}"

instance RenderJS JSImportSpecifier where
    (|>) pacc (JSImportSpecifier x1) = pacc |> x1
    (|>) pacc (JSImportSpecifierAs x1 annot x2) = pacc |> x1 |> annot |> "as" |> x2

instance RenderJS JSExportDeclaration where
    (|>) pacc (JSExport x1 s)             = pacc |> x1 |> s
    (|>) pacc (JSExportLocals xs semi)    = pacc |> xs |> semi
    (|>) pacc (JSExportFrom xs from semi) = pacc |> xs |> from |> semi

instance RenderJS JSExportClause where
    (|>) pacc (JSExportClause alb JSLNil arb) = pacc |> alb |> "{" |> arb |> "}"
    (|>) pacc (JSExportClause alb s arb) = pacc |> alb |> "{" |> s |> arb |> "}"

instance RenderJS JSExportSpecifier where
    (|>) pacc (JSExportSpecifier i) = pacc |> i
    (|>) pacc (JSExportSpecifierAs x1 annot x2) = pacc |> x1 |> annot |> "as" |> x2

instance RenderJS a => RenderJS (JSCommaList a) where
    (|>) pacc (JSLCons pl a i) = pacc |> pl |> a |> "," |> i
    (|>) pacc (JSLOne i)       = pacc |> i
    (|>) pacc JSLNil           = pacc

instance RenderJS a => RenderJS (JSCommaTrailingList a) where
    (|>) pacc (JSCTLComma xs a) = pacc |> xs |> a |> ","
    (|>) pacc (JSCTLNone xs)    = pacc |> xs

instance RenderJS JSIdent where
    (|>) pacc (JSIdentName a s) = pacc |> a |> s
    (|>) pacc JSIdentNone       = pacc

instance RenderJS (Maybe JSExpression) where
    (|>) pacc (Just e) = pacc |> e
    (|>) pacc Nothing  = pacc

instance RenderJS JSVarInitializer where
    (|>) pacc (JSVarInit a x) = pacc |> a |> "=" |> x
    (|>) pacc JSVarInitNone   = pacc

instance RenderJS [JSTemplatePart] where
    (|>) = foldl' (|>)

instance RenderJS JSTemplatePart where
    (|>) pacc (JSTemplatePart e a s) = pacc |> e |> a |> s

instance RenderJS JSClassHeritage where
    (|>) pacc (JSExtends a e) = pacc |> a |> "extends" |> e
    (|>) pacc JSExtendsNone   = pacc

instance RenderJS [JSClassElement] where
    (|>) = foldl' (|>)

instance RenderJS JSClassElement where
    (|>) pacc (JSClassInstanceMethod m) = pacc |> m
    (|>) pacc (JSClassStaticMethod a m) = pacc |> a |> "static" |> m
    (|>) pacc (JSClassSemi a)           = pacc |> a |> ";"

-- EOF
