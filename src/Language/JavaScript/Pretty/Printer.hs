module Language.JavaScript.Pretty.Printer (
  -- * Printing
  renderJS
  , renderToString
  ) where

--import Control.Applicative
--import Control.Monad (MonadPlus(..), ap)
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

import Debug.Trace

-- ---------------------------------------------------------------------

data Foo = Foo (Int,Int) BB.Builder
--data Foo a = Foom (Int,Int) a

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

-- ---------------------------------------------------------------------

renderJS :: JSNode -> BB.Builder
renderJS node = bb
  where
    Foo _ bb = rn node (Foo (1,1) empty)

-- Take in the current
-- rn :: (Int, Int) -> JSNode -> ((Int, Int), BB.Builder)
rn :: JSNode -> Foo -> Foo

rn (NS (JSSourceElementsTop xs) p cs) foo = rJS cs p xs foo
rn (NS (JSSourceElements    xs) p cs) foo = rJS cs p xs foo
rn (NS (JSExpression xs) p cs) foo        = rJS cs p xs foo
rn (NS (JSIdentifier s) p cs) foo         = rcs cs p s  foo
rn (NS (JSOperator n) p cs) foo           = rJS cs p [n] foo
rn (NS (JSDecimal i) p cs) foo            = rcs cs p i foo
rn (NS (JSLiteral l) p cs) foo            = rcs cs p l foo
rn (NS (JSUnary l n) p cs) foo            = rJS cs p [n] foo
rn (NS (JSHexInteger i) p cs) foo         = rcs cs p i foo
rn (NS (JSStringLiteral s l) p cs) foo    = rcs cs p ((s:l)++[s]) foo
rn (NS (JSRegEx s) p cs) foo              = rcs cs p s foo

rn (NS (JSArrayLiteral lb xs rb) p []) foo  = (rJS [] p [rb] (rJS  [] p xs (rJS [] p [lb] foo)))

rn (NS (JSElision xs) p cs) foo             = rJS [] p xs foo
rn (NS (JSStatementBlock lb x rb) p []) foo = (rJS [] p [rb] (rJS [] p [x] (rJS [] p [lb] foo)))
rn (NS (JSStatementList xs) p cs) foo       = rJS cs p xs foo
rn (NS (JSLabelled l c v) p cs) foo           = (rJS [] p [v] (rJS cs p [c] (rJS [] p [l] foo)))

rn (NS (JSObjectLiteral lb xs rb) p []) foo     = (rJS [] p [rb] (rJS  [] p xs (rJS [] p [lb] foo)))
rn (NS (JSPropertyNameandValue n colon vs) p []) foo  = (rJS [] p vs (rJS [] p [colon] (rJS [] p [n] foo)))

-- rn (NS (JSPropertyAccessor s n ps b) p cs) foo = (rcs [] p s (rcs [] p " " (rJS [] p [n] (rcs [] p ")" (rJS [] p ps (rcs [] p "(" (rJS [] p [b] foo)))))))

rn (NS (JSPropertyAccessor s n lb1 ps rb1 lb2 b rb2) p []) foo =
  (rJS [] p [rb2]
  (rJS [] p [b]
  (rJS [] p [lb2]
  (rJS [] p [rb1]
  (rJS [] p ps
  (rJS [] p [lb1]
  (rJS [] p [n]
  (rJS [] p [s] foo))))))))

-- NOTE: the braces are attached to the surrounding production
rn (NS (JSFunctionBody xs) p []) foo = (rJS [] p xs foo)


-- rn (JSReturn [])                 = (text "return")
-- rn (JSReturn [(NS (JSLiteral ";") _ _)])    = (text "return;")
-- rn (JSReturn xs)                 = (text "return") <> (if (spaceNeeded xs) then (text " ") else (empty)) <> (rJS xs)

rn (NS (JSReturn xs) p []) foo = (rJS [] p xs foo)

rn (NS (JSExpressionParen lb e rb) p []) foo   = (rJS [] p [rb] (rJS  [] p [e] (rJS [] p [lb] foo)))
rn (NS (JSExpressionTernary cond h v1 c v2) p []) foo  =  (rJS [] p v2
                                                           (rJS [] p [c]
                                                            (rJS [] p v1
                                                             (rJS [] p [h]
                                                              (rJS [] p cond foo)))))

rn (NS (JSExpressionBinary s lhs op rhs) p []) foo   = (rJS [] p rhs (rJS  [] p [op] (rJS [] p lhs foo)))
rn (NS (JSExpressionPostfix s xs op) p []) foo   = (rJS  [] p [op] (rJS [] p xs foo))

rn (NS (JSFunctionExpression f x1s lb x2s rb lb2 x3 rb2) p []) foo = (rJS [] p [rb2 ]
                                                                      (rJS [] p [x3]
                                                                       (rJS [] p [lb2]
                                                                        (rJS [] p [rb]
                                                                         (rJS [] p x2s
                                                                          (rJS [] p [lb]
                                                                           (rJS [] p x1s
                                                                            (rJS [] p [f] foo))))))))
rn (NS (JSMemberSquare xs lb e rb) p []) foo = (rJS [] p [rb]
                                                (rJS [] p [e]
                                                 (rJS [] p [lb]
                                                  (rJS [] p xs foo))))

rn (NS (JSMemberDot xs dot n) p []) foo = (rJS [] p [n]
                                           (rJS [] p [dot]
                                            (rJS [] p xs foo)))

rn (NS (JSArguments lb xs rb) p []) foo = (rJS [] p [rb]
                                           (rJS [] p xs
                                            (rJS [] p [lb] foo)))

rn (NS (JSCallExpression s os xs cs) p []) foo = (rJS [] p cs
                                                  (rJS [] p xs
                                                   (rJS [] p os foo)))

rn (NS (JSIf i lb x1 rb x2 x3s) p []) foo = (rJS [] p x3s
                                             (rJS [] p [x2]
                                              (rJS [] p [rb]
                                               (rJS [] p [x1]
                                                (rJS [] p [lb]
                                                 (rJS [] p [i] foo))))))

rn (NS (JSBlock lb x rb) p []) foo = (rJS [] p rb
                                      (rJS [] p [x]
                                       (rJS [] p lb foo)))

rn (NS (JSDoWhile d x1 w lb x2 rb x3) p []) foo = (rJS [] p [x3]
                                                   (rJS [] p [rb]
                                                    (rJS [] p [x2]
                                                     (rJS [] p [lb]
                                                      (rJS [] p [w]
                                                       (rJS [] p [x1]
                                                        (rJS [] p [d] foo)))))))

rn (NS (JSWhile w lb x1 rb x2) p []) foo = (rJS [] p [x2]
                                            (rJS [] p [rb]
                                             (rJS [] p [x1]
                                              (rJS [] p [lb]
                                               (rJS [] p [w] foo)))))

rn (NS (JSFor f lb x1s s1 x2s s2 x3s rb x4) p []) foo = (rJS [] p [x4]
                                                         (rJS [] p [rb]
                                                          (rJS [] p x3s
                                                           (rJS [] p [s2]
                                                            (rJS [] p x2s
                                                             (rJS [] p [s1]
                                                              (rJS [] p x1s
                                                               (rJS [] p [lb]
                                                                (rJS [] p [f] foo)))))))))

rn (NS (JSForVar f lb v x1s s1 x2s s2 x3s rb x4) p []) foo = (rJS [] p [x4]
                                                              (rJS [] p [rb]
                                                               (rJS [] p x3s
                                                                (rJS [] p [s2]
                                                                 (rJS [] p x2s
                                                                  (rJS [] p [s1]
                                                                   (rJS [] p x1s
                                                                    (rJS [] p [v]
                                                                     (rJS [] p [lb]
                                                                      (rJS [] p [f] foo))))))))))

rn (NS (JSForIn f lb x1s i x2 rb x3) p []) foo = (rJS [] p [x3]
                                                  (rJS [] p [rb]
                                                   (rJS [] p [x2]
                                                    (rJS [] p [i]
                                                     (rJS [] p x1s
                                                      (rJS [] p [lb]
                                                       (rJS [] p [f] foo)))))))

rn (NS (JSForVarIn f lb v x1 i x2 rb x3) p []) foo = (rJS [] p [x3]
                                                      (rJS [] p [rb]
                                                       (rJS [] p [x2]
                                                        (rJS [] p [i]
                                                         (rJS [] p [x1]
                                                          (rJS [] p [v]
                                                           (rJS [] p [lb]
                                                            (rJS [] p [f] foo))))))))

rn (NS (JSVarDecl x1 x2s) p []) foo = (rJS [] p x2s
                                       (rJS [] p [x1] foo))

rn (NS (JSVariables n xs as) p []) foo = (rJS [] p [as]
                                          (rJS [] p xs
                                           (rJS [] p [n] foo)))

rn (NS (JSContinue xs) p []) foo = (rJS [] p xs foo)

rn (NS (JSBreak x1s x2s) p []) foo = (rJS [] p x2s
                                      (rJS [] p x1s foo))

rn (NS (JSWith w lb x1 rb x2s) p []) foo = (rJS [] p x2s
                                            (rJS [] p [rb]
                                             (rJS [] p [x1]
                                              (rJS [] p [lb]
                                               (rJS [] p [w] foo)))))

rn (NS (JSSwitch s lb x rb x2s) p []) foo = (rJS [] p x2s
                                             (rJS [] p [rb]
                                              (rJS [] p [x]
                                               (rJS [] p [lb]
                                                (rJS [] p [s] foo)))))

rn (NS (JSThrow t x) p []) foo = (rJS [] p [x]
                                  (rJS [] p [t] foo))

rn (NS (JSCase ca x1 c x2) p []) foo = (rJS [] p [x2]
                                        (rJS [] p [c]
                                         (rJS [] p [x1]
                                          (rJS [] p [ca] foo))))

rn (NS (JSDefault d c x) p []) foo = (rJS [] p [x]
                                      (rJS [] p [c]
                                       (rJS [] p [d] foo)))

rn (NS (JSTry t x1 x2s) p []) foo = (rJS [] p x2s
                                     (rJS [] p [x1]
                                      (rJS [] p [t] foo)))

rn (NS (JSFunction f x1 lb x2s rb lb2 x3 rb2) p []) foo = (rJS [] p [rb2]
                                                           (rJS [] p [x3]
                                                            (rJS [] p [lb2]
                                                             (rJS [] p [rb]
                                                              (rJS [] p x2s
                                                               (rJS [] p [lb]
                                                                (rJS [] p [x1]
                                                                 (rJS [] p [f] foo))))))))

rn (NS (JSCatch c lb x1 x2s rb x3) p []) foo = (rJS [] p [x3]
                                                (rJS [] p [rb]
                                                 (rJS [] p x2s
                                                  (rJS [] p [x1]
                                                   (rJS [] p [lb]
                                                    (rJS [] p [c] foo))))))

rn (NS (JSFinally f x) p []) foo = (rJS [] p [x]
                                    (rJS [] p [f] foo))

-- Debug helper
rn what foo = rs (show what) foo

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


rJS :: [CommentAnnotation] -> TokenPosn -> [JSNode] -> Foo -> Foo
rJS cs p xs foo = foldl' (flip rn) (rc cs foo) xs


-- ++AZ++
{-
commaList :: [CommentAnnotation] -> TokenPosn -> [JSNode] -> Foo -> Foo
commaList _cs _p [] foo = foo
commaList cs p (x:xs) foo = go x xs
  where
    go y [] = rn y foo
    go y (z:zs) = (rn y foo <> comma) : go z zs

    -- (xs', trail) = if (last xs == JSLiteral ",") then (init xs, [comma]) else (xs,[])
    (xs', trail) = if (x' == JSLiteral ",") then (init xs, [comma]) else (xs,[])
    (NS x' _ _) = last xs
-}

renderToString :: JSNode -> String
renderToString js = map (\x -> chr (fromIntegral x)) $ LB.unpack $ BB.toLazyByteString $ renderJS js

--rn _ _ = undefined

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

