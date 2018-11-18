module Test.Language.Javascript.ModuleParser
    ( testModuleParser
    ) where

import Test.Hspec

import Language.JavaScript.Parser


testModuleParser :: Spec
testModuleParser = describe "Parse modules:" $
    it "export" $ do
        test "export {}"
            `shouldBe`
            "Right (JSAstModule [JSModuleExportDeclaration (JSExportLocals (()))])"
        test "export {};"
            `shouldBe`
            "Right (JSAstModule [JSModuleExportDeclaration (JSExportLocals (()))])"
        test "export const a = 1;"
            `shouldBe`
            "Right (JSAstModule [JSModuleExportDeclaration (JSExport (JSConstant (JSVarInitExpression (JSIdentifier 'a') [JSDecimal '1'])))])"
        test "export { a };"
            `shouldBe`
            "Right (JSAstModule [JSModuleExportDeclaration (JSExportLocals ((JSExportLocalSpecifier (JSIdentifier 'a'))))])"
        test "export { a as b };"
            `shouldBe`
            "Right (JSAstModule [JSModuleExportDeclaration (JSExportLocals ((JSExportLocalSpecifierAs (JSIdentifier 'a',JSIdentifier 'b'))))])"

test :: String -> String
test str = showStrippedMaybe (parseModule str "src")
