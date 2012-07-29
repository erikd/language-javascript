{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Requires package 'derive'.

import Control.Applicative
import Data.Derive.Arbitrary
import Data.DeriveTH
import Test.QuickCheck

import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST


import Debug.Trace


main :: IO ()
main = quickCheck (propertyParsableAST parseWrapper)

propertyParsableAST :: (String -> JSAST) -> TestAST -> Bool
propertyParsableAST parser (TestAST ast) =
    let parsed = parser (renderToString ast)
    in showStripped parsed == showStripped (trace ("\n" ++ showStripped ast ++ "\n" ++ showStripped parsed ++ "\n\n") ast)

parseWrapper :: String -> JSAST
parseWrapper code =
    if null code
        then JSSourceElementsTop []
        else case parse code "generated javascript" of
                Left _ -> JSSourceElementsTop []
                Right tree -> tree

-- Use a newtype wrapper so we can define a Show instance that renders the
-- AST to Javascript.
newtype TestAST
    = TestAST JSAST
    deriving Eq

instance Show TestAST where
    show (TestAST ast) = "-------\n" ++ renderToString ast ++ "\n-------"

--------------------------------------------------------------------------------
-- QuickCheck Arbitrary instances.

instance Arbitrary TestAST where
    arbitrary = TestAST <$> arbitrary


instance Arbitrary JSAnnot where
    -- Always stick in whitespace.
    arbitrary = return $ JSAnnot tokenPosnEmpty [ WhiteSpace tokenPosnEmpty " "]


instance Arbitrary JSSemi where
    -- The generated test code should always use semicolons.
    arbitrary = JSSemi <$> arbitrary


instance Arbitrary JSNode where
    arbitrary = sizeLimited jsNode

jsNode :: Int -> Gen JSNode
jsNode 0 =
    oneof
        [ JSDecimal <$> arbitrary <*> arbDecimalString
        , JSIdentifier <$> arbitrary <*> arbIdentifier
        ]
jsNode n =
        oneof
            [ JSUnaryExpression <$> arbitrary <*> jsNode (n - 1)
            , JSExpressionBinary <$> jsNode (n - 1) <*> arbitrary <*> jsNode (n - 1)
            ]


instance Arbitrary JSStatement where
    arbitrary = sizeLimited jsStatement

jsStatement :: Int -> Gen JSStatement
jsStatement 0 =
    oneof
        [ JSExpressionStatement <$> arbitrary <*> arbitrary
        , JSThrow <$> arbitrary <*> arbitrary
        ]
jsStatement _ =
        oneof
            [ JSExpressionStatement <$> arbitrary <*> arbitrary
            , JSThrow <$> arbitrary <*> arbitrary
            ]

--------------------------------------------------------------------------------
-- A couple more helper generators.

sizeLimited :: (Int -> Gen a) -> Gen a
sizeLimited f = do
    x <- arbitrary
    f (max (abs x) 2)

arbDecimalString :: Gen String
arbDecimalString = arbDecimal >>= \i -> return (show $ abs i)
  where
    arbDecimal :: Gen Integer
    arbDecimal = arbitrary

arbIdentifier :: Gen String
arbIdentifier = do
    s <- choose ('a', 'z')
    ss <- oneof [ choose ('a', 'z'), choose ('0', '9') ]
    return [ s, ss ]

--------------------------------------------------------------------------------
-- Use template Haskell to automagically derive Aribitrary instances for these.

$(derive makeArbitrary ''JSBinOp)
$(derive makeArbitrary ''JSUnaryOp)
$(derive makeArbitrary ''JSAST)
