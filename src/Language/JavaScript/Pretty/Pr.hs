module Language.JavaScript.Pretty.Pr (
  -- * Printing
  renderJS
  -- , renderToString
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

rn (Foo (r,c) bb) (NS (JSIdentifier s) p cs) = Foo (r,c+length s) (text s)

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

