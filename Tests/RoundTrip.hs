module Tests.RoundTrip
    ( testRoundTrip
    ) where

import Test.Hspec

import Language.JavaScript.Parser


testRoundTrip :: Spec
testRoundTrip = describe "Roundtrip:" $ do
    it "multi comment" $ do
        testRT "/*a*/\n//foo\nnull"
        testRT "/*a*/x"
        testRT "/*a*/null"
        testRT "/*b*/false"
        testRT "true/*c*/"
        testRT "/*c*/true"
        testRT "/*d*/0x1234fF"
        testRT "/*e*/1.0e4"
        testRT "/*x*/011"
        testRT "/*f*/\"hello\\nworld\""
        testRT "/*g*/'hello\\nworld'"
        testRT "/*h*/this"
        testRT "/*i*//blah/"
        testRT "//j\nthis_"

    it "arrays" $ do
        testRT "/*a*/[/*b*/]"
        testRT "/*a*/[/*b*/,/*c*/]"
        testRT "/*a*/[/*b*/,/*c*/,/*d*/]"
        testRT "/*a*/[/*b/*,/*c*/,/*d*/x/*e*/]"
        testRT "/*a*/[/*b*/,/*c*/,/*d*/x/*e*/]"
        testRT "/*a*/[/*b*/,/*c*/x/*d*/,/*e*/,/*f*/x/*g*/]"
        testRT "/*a*/[/*b*/x/*c*/]"
        testRT "/*a*/[/*b*/x/*c*/,/*d*/]"

    it "object literals" $ do
        testRT "/*a*/{/*b*/}"
        testRT "/*a*/{/*b*/x/*c*/:/*d*/1/*e*/}"
        testRT "x=/*a*/{/*b*/x/*c*/:/*d*/1/*e*/,/*f*/y/*g*/:/*h*/2/*i*/}"
        testRT "x=/*a*/{/*b*/x/*c*/:/*d*/1/*e*/,/*f*/y/*g*/:/*h*/2/*i*/,/*j*/z/*k*/:/*l*/3/*m*/}"
        testRT "a=/*a*/{/*b*/x/*c*/:/*d*/1/*e*/,/*f*/}"

    it "miscellaneous" $ do
        testRT "/*a*/(/*b*/56/*c*/)"
        testRT "/*a*/true/*b*/?/*c*/1/*d*/:/*e*/2"
        testRT "/*a*/x/*b*/||/*c*/y"
        testRT "/*a*/x/*b*/&&/*c*/y"
        testRT "/*a*/x/*b*/|/*c*/y"
        testRT "/*a*/x/*b*/^/*c*/y"
        testRT "/*a*/x/*b*/&/*c*/y"
        testRT "/*a*/x/*b*/==/*c*/y"
        testRT "/*a*/x/*b*/!=/*c*/y"
        testRT "/*a*/x/*b*/===/*c*/y"
        testRT "/*a*/x/*b*/!==/*c*/y"
        testRT "/*a*/x/*b*/</*c*/y"
        testRT "/*a*/x/*b*/>/*c*/y"
        testRT "/*a*/x/*b*/<=/*c*/y"
        testRT "/*a*/x/*b*/>=/*c*/y"
        testRT "/*a*/x /*b*/instance of /*c*/y"
        testRT "/*a*/x/*b*/=/*c*/{/*d*/get/*e*/ foo/*f*/(/*g*/)/*h*/ {/*i*/return/*j*/ 1/*k*/}/*l*/,/*m*/set/*n*/ foo/*o*/(/*p*/a/*q*/) /*r*/{/*s*/x/*t*/=/*u*/a/*v*/}/*w*/}"


testRT :: String -> Expectation
testRT str = str `shouldBe` renderToString (readJs str)

