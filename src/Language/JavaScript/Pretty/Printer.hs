module Language.JavaScript.Pretty.Printer (
  -- * Printing
  renderJS
  , renderToString
  ) where

--import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
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
rn (NS (JSIdentifier s) p cs) foo         = rcs cs p s foo
rn (NS (JSOperator s) p cs) foo           = rcs cs p s foo
rn (NS (JSDecimal i) p cs) foo            = rcs cs p i foo
rn (NS (JSLiteral l) p cs) foo            = rcs cs p l foo
rn (NS (JSUnary l) p cs) foo              = rcs cs p l foo
rn (NS (JSHexInteger i) p cs) foo         = rcs cs p i foo
rn (NS (JSStringLiteral s l) p cs) foo    = rcs cs p ((s:l)++[s]) foo
rn (NS (JSRegEx s) p cs) foo              = rcs cs p s foo

rn (NS (JSArrayLiteral xs) p [c1,c2]) foo  = (rcs [c2] p "]" (rJS  [] p xs (rcs [c1] p "[" foo)))

rn (NS (JSElision xs) p cs) foo            = rJS [] p xs (rcs cs p "," foo)
rn (NS (JSStatementBlock x) p [c1,c2]) foo = (rcs [c2] p "}" (rJS [] p [x] (rcs [c1] p "{" foo)))
rn (NS (JSStatementList xs) p cs) foo      = rJS cs p xs foo
rn (NS (JSLabelled l v) p cs) foo          = (rJS [] p [v] (rcs cs p ":" (rJS [] p [l] foo)))

rn (NS (JSObjectLiteral xs) p [c1,c2]) foo      = (rcs [c2] p "}" (rJS  [] p xs (rcs [c1] p "{" foo)))
rn (NS (JSPropertyNameandValue n vs) p cs) foo  = (rJS [] p vs (rcs cs p ":" (rJS [] p [n] foo)))

-- rn (NS (JSPropertyAccessor s n ps b) p cs) foo = (rcs [] p s (rcs [] p " " (rJS [] p [n] (rcs [] p ")" (rJS [] p ps (rcs [] p "(" (rJS [] p [b] foo)))))))

rn (NS (JSPropertyAccessor s n ps b) p cs) foo = (rJS [] p [b]
                                                 (rcs [] p ")"
                                                 (rJS [] p ps
                                                 (rcs [] p "("
                                                 (rJS [] p [n]
                                                 (rcs [] p " "
                                                 (rcs [] p s foo)))))))

-- NOTE: the comments for the brackets are attached to JSFunction. Oops.
rn (NS (JSFunctionBody xs) p cs) foo = (rcs [] p "}" (rJS [] p xs (rcs cs p "{" foo)))


-- rn (JSReturn [])                 = (text "return")
-- rn (JSReturn [(NS (JSLiteral ";") _ _)])    = (text "return;")
-- rn (JSReturn xs)                 = (text "return") <> (if (spaceNeeded xs) then (text " ") else (empty)) <> (rJS xs)

rn (NS (JSReturn [])                            p cs) foo = (rcs cs p "return" foo)
rn (NS (JSReturn [(NS (JSLiteral ";") p1 cs1)]) p cs) foo = (rcs cs1 p1 ";" (rcs cs p "return" foo))
rn (NS (JSReturn xs)                            p cs) foo = (rJS [] p xs (rcs cs p "return" foo))

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


rps :: TokenPosn -> String -> Foo -> Foo
rps p s foo = (rs s foo')
  where
    foo' = (goto p foo)

-- Render a string
rs :: String -> Foo -> Foo
rs s (Foo (r,c) bb) = (Foo (r',c') (bb <> (text s)))
  where
    (r',c') = foldl' (\(row,col) char -> go (row,col) char) (r,c) s

    go (r,c) '\n' = (r+1,0)
    go (r,c) _    = (r,c+1)


goto :: TokenPosn -> Foo -> Foo
goto (TokenPn _ ltgt ctgt) (Foo (lcur,ccur) bb) = (Foo (lnew,cnew) (bb <> bb'))
  where
    lnew = if (lcur < ltgt) then ltgt else lcur
    cnew = if (ccur < ctgt) then ctgt else ccur
    bbline = if (lcur < ltgt) then (text $ take (ltgt - lcur) $ repeat '\n') else mempty
    bbcol  = if (ccur < ctgt) then (text $ take (ctgt - ccur) $ repeat ' ' ) else mempty
    bb' = bbline <> bbcol


rJS :: [CommentAnnotation] -> TokenPosn -> [JSNode] -> Foo -> Foo
rJS cs p xs foo = foldl' (flip rn) (rc cs foo) xs

{-
-- A space is needed if this expression starts with an identifier etc, but not if with a '('
spaceNeeded :: [JSNode] -> Bool
spaceNeeded xs =
  let
   -- str = show $ rJS xs
    str = LB.unpack $ BB.toLazyByteString $ rJS xs
  in
   head str /= (fromIntegral $ ord '(')
-}



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

{-
-- From pretty print library
punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []     = []
punctuate p (x:xs) = go x xs
                   where go y []     = [y]
                         go y (z:zs) = (y <> p) : go z zs
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

