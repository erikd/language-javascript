{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.JavaScript.Transform.Basic (
  ) where

import Control.Applicative
import Data.List
import Data.Functor
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.SrcLocation
import Language.JavaScript.Parser.Token

-- ---------------------------------------------------------------------

class TransformJS a where
    -- Transfrom node.
    tf :: a -> a

instance TransformJS JSAST where
  tf (JSSourceElementsTop xs) = JSSourceElementsTop (tf xs)


instance TransformJS JSStatement where
    tf (JSStatementBlock blk)                       = (JSStatementBlock (tf blk))
    tf (JSBreak annot x1s s)                        = (JSBreak (tf annot) (tf x1s) (tf s))
    tf (JSContinue annot xs s)                      = (JSContinue (tf annot) (tf xs) (tf s))
    tf (JSConstant annot xs s)                      = (JSConstant (tf annot) (tf xs) (tf s))
    tf (JSDoWhile ad x1 aw alb x2 arb x3)           = (JSDoWhile (tf ad) (tf x1) (tf aw) (tf alb) (tf x2) (tf arb) (tf x3))
    tf (JSFor af alb x1s s1 x2s s2 x3s arb x4)      = (JSFor (tf af) (tf alb) (tf x1s) (tf s1) (tf x2s) (tf s2) (tf x3s) (tf arb) (tf x4))
    tf (JSForIn af alb x1s i x2 arb x3)             = (JSForIn (tf af) (tf alb) (tf x1s) (tf i) (tf x2) (tf arb) (tf x3))
    tf (JSForVar af alb v x1s s1 x2s s2 x3s arb x4) = (JSForVar (tf af) (tf alb) (tf v) (tf x1s) (tf s1) (tf x2s) (tf s2) (tf x3s) (tf arb) (tf x4))
    tf (JSForVarIn af alb v x1 i x2 arb x3)         = (JSForVarIn (tf af) (tf alb) (tf v) (tf x1) (tf i) (tf x2) (tf arb) (tf x3))
    tf (JSFunction af n alb x2s arb x3)             = (JSFunction (tf af) (tf n) (tf alb) (tf x2s) (tf arb) (tf x3))
    tf (JSIf annot alb x1 arb x2s x3s)              = (JSIf (tf annot) (tf alb) (tf x1) (tf arb) (tf x2s) (tf x3s))
    tf (JSLabelled l c v)                           = (JSLabelled (tf l) (tf c) (tf v))
    tf (JSExpressionStatement l)                    = (JSExpressionStatement (tf l))
    tf (JSReturn annot xs s)                        = (JSReturn (tf annot) (tf xs) (tf s))
    tf (JSSwitch annot alp x arp alb x2 arb)        = (JSSwitch (tf annot) (tf alp) (tf x) (tf arp) (tf alb) (tf x2) (tf arb))
    tf (JSThrow annot x)                            = (JSThrow (tf annot) (tf x))
    tf (JSTry annot tb tcs tfi)                     = (JSTry (tf annot) (tf tb) (tf tcs) (tf tfi))
    tf (JSVarDecl x1 x2s)                           = (JSVarDecl (tf x1) (tf x2s))
    tf (JSVariable annot xs s)                      = (JSVariable (tf annot) (tf xs) (tf s))
    tf (JSWhile annot alp x1 arp x2)                = (JSWhile (tf annot) (tf alp) (tf x1) (tf arp) (tf x2))
    tf (JSWith annot alp x1 arp x s)                = (JSWith (tf annot) (tf alp) (tf x1) (tf arp) (tf x) (tf s))


instance TransformJS [JSStatement] where
    tf xs = map tf xs


instance TransformJS JSBinOp where
    tf (JSBinOpAnd        annot)  = (JSBinOpAnd        (tf annot))
    tf (JSBinOpBitAnd     annot)  = (JSBinOpBitAnd     (tf annot))
    tf (JSBinOpBitOr      annot)  = (JSBinOpBitOr      (tf annot))
    tf (JSBinOpBitXor     annot)  = (JSBinOpBitXor     (tf annot))
    tf (JSBinOpDivide     annot)  = (JSBinOpDivide     (tf annot))
    tf (JSBinOpEq         annot)  = (JSBinOpEq         (tf annot))
    tf (JSBinOpGe         annot)  = (JSBinOpGe         (tf annot))
    tf (JSBinOpGt         annot)  = (JSBinOpGt         (tf annot))
    tf (JSBinOpIn         annot)  = (JSBinOpIn         (tf annot))
    tf (JSBinOpInstanceOf annot)  = (JSBinOpInstanceOf (tf annot))
    tf (JSBinOpLe         annot)  = (JSBinOpLe         (tf annot))
    tf (JSBinOpLsh        annot)  = (JSBinOpLsh        (tf annot))
    tf (JSBinOpLt         annot)  = (JSBinOpLt         (tf annot))
    tf (JSBinOpMinus      annot)  = (JSBinOpMinus      (tf annot))
    tf (JSBinOpMod        annot)  = (JSBinOpMod        (tf annot))
    tf (JSBinOpNeq        annot)  = (JSBinOpNeq        (tf annot))
    tf (JSBinOpOr         annot)  = (JSBinOpOr         (tf annot))
    tf (JSBinOpPlus       annot)  = (JSBinOpPlus       (tf annot))
    tf (JSBinOpRsh        annot)  = (JSBinOpRsh        (tf annot))
    tf (JSBinOpStrictEq   annot)  = (JSBinOpStrictEq   (tf annot))
    tf (JSBinOpStrictNeq  annot)  = (JSBinOpStrictNeq  (tf annot))
    tf (JSBinOpTimes      annot)  = (JSBinOpTimes      (tf annot))
    tf (JSBinOpUrsh       annot)  = (JSBinOpUrsh       (tf annot))

instance TransformJS JSSwitchParts where
    tf (JSCase    annot x1 c x2s) = (JSCase    (tf annot) (tf x1) (tf c) (tf x2s))
    tf (JSDefault annot c xs)     = (JSDefault (tf annot) (tf c) (tf xs))

instance TransformJS [JSSwitchParts] where
    tf xs = map tf xs

instance TransformJS JSBlock where
    tf (JSBlock alb ss arb) = (JSBlock (tf alb) (tf ss) (tf arb))

instance TransformJS JSTryCatch where
    tf (JSCatch        anc alb x1 x2s arb x3) = (JSCatch  (tf anc) (tf alb) (tf x1) (tf x2s) (tf arb) (tf x3))

instance TransformJS [JSTryCatch] where
    tf xs = map tf xs

instance TransformJS JSTryFinally where
    tf (JSFinally      annot x) = (JSFinally      (tf annot) (tf x))
    tf JSNoFinally              = JSNoFinally


instance TransformJS JSNode where
    -- Terminals
    tf (JSIdentifier    annot s  ) = (JSIdentifier    (tf annot) (tf s)  )
    tf (JSDecimal       annot i  ) = (JSDecimal       (tf annot) (tf i)  )
    tf (JSLiteral       annot l  ) = (JSLiteral       (tf annot) (tf l)  )
    tf (JSHexInteger    annot i  ) = (JSHexInteger    (tf annot) (tf i)  )
    tf (JSOctal         annot i  ) = (JSOctal         (tf annot) (tf i)  )
    tf (JSStringLiteral annot s l) = (JSStringLiteral (tf annot) (tf s) (tf l))
    tf (JSRegEx         annot s  ) = (JSRegEx         (tf annot) (tf s)  )

    -- Non-Terminals
    tf (JSArguments            alp xs arp)             = (JSArguments            (tf alp) (tf xs) (tf arp))
    tf (JSArrayLiteral         als xs ars)             = (JSArrayLiteral         (tf als) (tf xs) (tf ars))
    tf (JSAssignExpression     lhs op rhs)             = (JSAssignExpression     (tf lhs) (tf op) (tf rhs))
    tf (JSCallExpression       os xs cs)               = (JSCallExpression       (tf os) (tf xs) (tf cs))
    tf (JSCallExpressionDot    os xs)                  = (JSCallExpressionDot    (tf os) (tf xs))
    tf (JSCallExpressionSquare als xs ars)             = (JSCallExpressionSquare (tf als) (tf xs) (tf ars))
    tf (JSElision              c)                      = (JSElision              (tf c))
    tf (JSExpression           xs)                     = (JSExpression           (tf xs))
    tf (JSExpressionBinary     lhs op rhs)             = (JSExpressionBinary     (tf lhs) (tf op) (tf rhs))
    tf (JSExpressionParen      alp e arp)              = (JSExpressionParen      (tf alp) (tf e) (tf arp))
    tf (JSExpressionPostfix    xs op)                  = (JSExpressionPostfix    (tf xs) (tf op))
    tf (JSExpressionTernary    cond h v1 c v2)         = (JSExpressionTernary    (tf cond) (tf h) (tf v1) (tf c) (tf v2))
    tf (JSFunctionExpression   annot x1s lb x2s rb x3) = (JSFunctionExpression   (tf annot) (tf x1s) (tf lb) (tf x2s) (tf rb) (tf x3))
    tf (JSMemberDot            xs dot n)               = (JSMemberDot            (tf xs) (tf dot) (tf n))
    tf (JSMemberSquare         xs als e ars)           = (JSMemberSquare         (tf xs) (tf als) (tf e) (tf ars))
    tf (JSObjectLiteral        alb xs arb)             = (JSObjectLiteral        (tf alb) (tf xs) (tf arb))
    tf (JSOpAssign             n)                      = (JSOpAssign             (tf n))
    tf (JSPropertyAccessor     s n alp ps arp b)       = (JSPropertyAccessor     (tf s) (tf n) (tf alp) (tf ps) (tf arp) (tf b))
    tf (JSPropertyNameandValue n colon vs)             = (JSPropertyNameandValue (tf n) (tf colon) (tf vs))
    tf (JSUnaryExpression      op x)                   = (JSUnaryExpression      (tf op) (tf x))


instance TransformJS [JSNode] where
    tf xs = map tf xs

instance TransformJS JSAnnot where
    tf (JSAnnot p cs) = (JSAnnot (tf p) (tf cs))
    tf JSNoAnnot      = JSNoAnnot

instance TransformJS JSSemi where
    tf (JSSemi annot) = (JSSemi (tf annot))
    tf JSSemiAuto     = JSSemiAuto


instance TransformJS JSUnaryOp where
    tf (JSUnaryOpDecr   annot) = (JSUnaryOpDecr   (tf annot))
    tf (JSUnaryOpDelete annot) = (JSUnaryOpDelete (tf annot))
    tf (JSUnaryOpIncr   annot) = (JSUnaryOpIncr   (tf annot))
    tf (JSUnaryOpMinus  annot) = (JSUnaryOpMinus  (tf annot))
    tf (JSUnaryOpNot    annot) = (JSUnaryOpNot    (tf annot))
    tf (JSUnaryOpPlus   annot) = (JSUnaryOpPlus   (tf annot))
    tf (JSUnaryOpTilde  annot) = (JSUnaryOpTilde  (tf annot))
    tf (JSUnaryOpTypeof annot) = (JSUnaryOpTypeof (tf annot))
    tf (JSUnaryOpVoid   annot) = (JSUnaryOpVoid   (tf annot))


instance TransformJS JSAssignOp where
    tf (JSAssign       annot) = (JSAssign       (tf annot))
    tf (JSTimesAssign  annot) = (JSTimesAssign  (tf annot))
    tf (JSDivideAssign annot) = (JSDivideAssign (tf annot))
    tf (JSModAssign    annot) = (JSModAssign    (tf annot))
    tf (JSPlusAssign   annot) = (JSPlusAssign   (tf annot))
    tf (JSMinusAssign  annot) = (JSMinusAssign  (tf annot))
    tf (JSLshAssign    annot) = (JSLshAssign    (tf annot))
    tf (JSRshAssign    annot) = (JSRshAssign    (tf annot))
    tf (JSUrshAssign   annot) = (JSUrshAssign   (tf annot))
    tf (JSBwAndAssign  annot) = (JSBwAndAssign  (tf annot))
    tf (JSBwXorAssign  annot) = (JSBwXorAssign  (tf annot))
    tf (JSBwOrAssign   annot) = (JSBwOrAssign   (tf annot))

instance TransformJS TokenPosn where
    tf (TokenPn x ltgt ctgt) = (TokenPn x ltgt ctgt)


instance TransformJS [CommentAnnotation] where
    tf xs = map tf xs


instance TransformJS CommentAnnotation where
    tf  NoComment = NoComment
    tf (CommentA   p s) = (CommentA   (tf p) (tf s))
    tf (WhiteSpace p s) = (WhiteSpace (tf p) (tf s))


instance TransformJS String where
    tf  s = s

instance TransformJS Char where
    tf  c = c






-- EOF

