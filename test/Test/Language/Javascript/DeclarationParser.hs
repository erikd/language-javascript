module Test.Language.Javascript.DeclarationParser
    ( testDeclarationParser
    ) where


import Test.Hspec

import Language.JavaScript.Parser
import Language.JavaScript.Parser.Grammar7
import Language.JavaScript.Parser.Parser


testDeclarationParser :: Spec
testDeclarationParser = describe "Parse declarations:" $ do
    it "export" $ do
        testDecl "export {}"    `shouldBe` "Right (JSAstDeclaration (JSExport))"
        testDecl "export {};"   `shouldBe` "Right (JSAstDeclaration (JSExport))"

testDecl :: String -> String
testDecl str = showStrippedMaybe (parseUsing parseDeclaration str "src")
