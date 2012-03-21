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
    Foo _ bb = rn (Foo (1,1) empty) node

-- Take in the current
-- rn :: (Int, Int) -> JSNode -> ((Int, Int), BB.Builder)
rn :: Foo -> JSNode -> Foo

rn foo (NS (JSSourceElementsTop xs) p cs) = rJS foo cs p xs

rn foo (NS (JSExpression xs) p cs)        = rJS foo cs p xs
rn foo (NS (JSIdentifier s) p cs)         = rcs foo cs p s
rn foo (NS (JSOperator s) p cs)           = rcs foo cs p s
rn foo (NS (JSDecimal i) p cs)            = rcs foo cs p i
rn foo (NS (JSLiteral l) p cs)            = rcs foo cs p l
rn foo (NS (JSUnary l) p cs)              = rcs foo cs p l
rn foo (NS (JSHexInteger i) p cs)         = rcs foo cs p i
rn foo (NS (JSStringLiteral s l) p cs)    = rcs foo cs p ((s:l)++[s])
rn foo (NS (JSRegEx s) p cs)              = rcs foo cs p s
rn foo (NS (JSArrayLiteral xs) p [c1,c2]) = (rcs (rJS (rcs foo [c1] p "[") [] p xs) [c2] p "]")
rn foo (NS (JSElision xs) p cs)           = rJS (rcs foo cs p ",") [] p xs
rn foo (NS (JSStatementBlock x) p [c1,c2]) = (rcs (rJS (rcs foo [c1] p "{") [] p [x]) [c2] p "}")
rn foo (NS (JSStatementList xs) p cs)      = rJS foo cs p xs
rn foo (NS (JSLabelled l v) p cs)          = (rJS (rs (rJS foo [] p [l]) ":") cs p [v])

-- ---------------------------------------------------------------------
-- Helper functions

-- ---------------------------------------------------------------------
-- Need a function that
-- a) renders all comments, according to their positions
-- b) advances to the position of the required string
-- c) renders the string, advancing the position
rcs :: Foo -> [CommentAnnotation] -> TokenPosn -> String -> Foo
rcs foo cs p s = rps (rc foo cs) p s

rc :: Foo -> [CommentAnnotation] -> Foo
rc foo cs = foldl' go foo cs
  where
    go :: Foo -> CommentAnnotation -> Foo
    go foo NoComment = foo
    go foo (CommentA p s) = rps foo p s


rps :: Foo -> TokenPosn -> String -> Foo
rps foo p s = (rs foo' s)
  where
    foo' = (goto foo p)

-- Render a string
rs :: Foo -> String -> Foo
rs (Foo (r,c) bb) s = (Foo (r',c') (bb <> (text s)))
  where
    (r',c') = foldl' (\(row,col) char -> go (row,col) char) (r,c) s

    go (r,c) '\n' = (r+1,0)
    go (r,c) _    = (r,c+1)


goto :: Foo -> TokenPosn -> Foo
goto (Foo (lcur,ccur) bb) (TokenPn _ ltgt ctgt) = (Foo (lnew,cnew) (bb <> bb'))
  where
    lnew = if (lcur < ltgt) then ltgt else lcur
    cnew = if (ccur < ctgt) then ctgt else ccur
    bbline = if (lcur < ltgt) then (text $ take (ltgt - lcur) $ repeat '\n') else mempty
    bbcol  = if (ccur < ctgt) then (text $ take (ctgt - ccur) $ repeat ' ' ) else mempty
    bb' = bbline <> bbcol


rJS :: Foo -> [CommentAnnotation] -> TokenPosn -> [JSNode] -> Foo
rJS foo cs p xs = foldl' rn (rc foo cs) xs

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

