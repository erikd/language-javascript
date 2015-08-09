
module Language.JavaScript.Parser.Traversals where

import Control.Applicative
import Control.Monad

import Language.JavaScript.Parser.AST

innerNode :: Applicative f => (Node -> f Node) -> JSNode -> f JSNode
innerNode f n = case n of
  NN n'          -> NN <$> f n'
  NT n' pos anns -> NT <$> f n' <*> pure pos <*> pure anns

everywhereOnJSNodesM :: (Applicative m, Monad m) => (JSNode -> m JSNode) -> JSNode -> m JSNode
everywhereOnJSNodesM f = go
  where
  go = innerNode go' >=> f
  go_ = mapM go

  go' node = case node of
    JSIdentifier{}    -> return node
    JSDecimal{}       -> return node
    JSLiteral{}       -> return node
    JSHexInteger{}    -> return node
    JSOctal{}         -> return node
    JSStringLiteral{} -> return node
    JSRegEx{}         -> return node

    JSArguments lb args rb ->
      JSArguments <$> go lb <*> go_ args <*> go rb
    JSArrayLiteral lb contents rb ->
      JSArrayLiteral <$> go lb <*> go_ contents <*> go rb
    JSBlock lb stmts rb ->
      JSBlock <$> go_ lb <*> go_ stmts <*> go_ rb
    JSBreak break_ ids autosemi ->
      JSBreak <$> go break_ <*> go_ ids <*> go autosemi
    JSCallExpression ty opens contents closes ->
      JSCallExpression ty <$> go_ opens <*> go_ contents <*> go_ closes
    JSCase case_ expr colon stmts ->
      JSCase <$> go case_ <*> go expr <*> go colon <*> go_ stmts
    JSCatch lb ident if_ exprs rb block ->
      JSCatch <$> go lb <*> go ident <*> go if_ <*> go_ exprs <*> go rb <*> go block
    JSContinue cont ids autosemi ->
      JSContinue <$> go cont <*> go_ ids <*> go autosemi
    JSDefault def colon stmts ->
      JSDefault <$> go def <*> go colon <*> go_ stmts
    JSDoWhile do_ stmt while lb exprs rb autosemi ->
      JSDoWhile <$> go do_ <*> go stmt <*> go while <*> go lb <*> go exprs <*> go rb <*> go autosemi
    JSElision comma ->
      JSElision <$> go comma
    JSExpression exprs ->
      JSExpression <$> go_ exprs
    JSExpressionBinary what lhs op rhs ->
      JSExpressionBinary what <$> go_ lhs <*> go op <*> go_ rhs
    JSExpressionParen lb expr rb ->
      JSExpressionParen <$> go lb <*> go expr <*> go rb
    JSExpressionPostfix ty exprs op ->
      JSExpressionPostfix ty <$> go_ exprs <*> go op
    JSExpressionTernary cond qmark ifTrue colon ifFalse ->
      JSExpressionTernary <$> go_ cond <*> go qmark <*> go_ ifTrue <*> go colon <*> go_ ifFalse
    JSFinally finally block ->
      JSFinally <$> go finally <*> go block
    JSFor for lb es1 s1 es2 s2 es3 rb stmt ->
      JSFor <$> go for <*> go lb <*> go_ es1 <*> go s1 <*> go_ es2 <*> go s2 <*> go_ es3 <*> go rb <*> go stmt 
    JSForIn for lb exprs1 in_ expr2 rb stmts ->
      JSForIn <$> go for <*> go lb <*> go_ exprs1 <*> go in_ <*> go expr2 <*> go rb <*> go stmts
    JSForVar for lb var vardecl s1 es2 s2 es3 rb stmt ->
      JSForVar <$> go for <*> go lb <*> go var <*> go_ vardecl <*> go s1 <*> go_ es2 <*> go s2 <*> go_ es3 <*> go rb <*> go stmt 
    JSForVarIn for lb var vardecl in_ expr2 rb stmts ->
      JSForVarIn <$> go for <*> go lb <*> go var <*> go vardecl <*> go in_ <*> go expr2 <*> go rb <*> go stmts
    JSFunction fn name lb params rb block ->
      JSFunction <$> go fn <*> go name <*> go lb <*> go_ params <*> go rb <*> go block
    JSFunctionExpression fn names lb params rb block ->
      JSFunctionExpression <$> go fn <*> go_ names <*> go lb <*> go_ params <*> go rb <*> go block
    JSIf if_ lb cond rb thens elses ->
      JSIf <$> go if_ <*> go lb <*> go cond <*> go rb <*> go_ thens <*> go_ elses
    JSLabelled id_ colon stmt ->
      JSLabelled <$> go id_ <*> go colon <*> go stmt
    JSMemberDot firsts dot name ->
      JSMemberDot <$> go_ firsts <*> go dot <*> go name
    JSMemberSquare firsts lb expr rb ->
      JSMemberSquare <$> go_ firsts <*> go lb <*> go expr <*> go rb
    JSObjectLiteral lb props rb ->
      JSObjectLiteral <$> go lb <*> go_ props <*> go rb
    JSOperator opnode ->
      JSOperator <$> go opnode
    JSPropertyAccessor getset name lb params rb block ->
      JSPropertyAccessor <$> go getset <*> go name <*> go lb <*> go_ params <*> go rb <*> go block
    JSPropertyNameandValue name colon value ->
      JSPropertyNameandValue <$> go name <*> go colon <*> go_ value
    JSReturn return_ exprs autosemi ->
      JSReturn <$> go return_ <*> go_ exprs <*> go autosemi
    JSSourceElementsTop elems ->
      JSSourceElementsTop <$> go_ elems
    JSSwitch switch lb expr rb case_ ->
      JSSwitch <$> go switch <*> go lb <*> go expr <*> go rb <*> go case_
    JSThrow throw expr ->
      JSThrow <$> go throw <*> go expr
    JSTry try block rest ->
      JSTry <$> go try <*> go block <*> go_ rest
    JSUnary ty op ->
      JSUnary ty <$> go op
    JSVarDecl ident inits ->
      JSVarDecl <$> go ident <*> go_ inits
    JSVariables varconst decls autosemi ->
      JSVariables <$> go varconst <*> go_ decls <*> go autosemi
    JSWhile while lb expr rb stmt ->
      JSWhile <$> go while <*> go lb <*> go expr <*> go rb <*> go stmt
    JSWith with lb expr rb stmts ->
      JSWith <$> go with <*> go lb <*> go expr <*> go rb <*> go_ stmts
