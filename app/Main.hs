import Language.JavaScript.Parser.Parser
import Language.JavaScript.Process.Minify
import Typechecker.Typechecker

{-
  this is stupid, we've banned module.exports
  but we need to export something
  so we just export our main function and nothing else
  its job is to do the exporting
-}
extraShit :: String
extraShit = "\n\nmodule.exports = { main: main }"

main :: IO ()
main = do
  file <- readFile "./samples/1.fjs"
  let response = parse file "1.js"
  case response of
    Left e -> print e
    Right a -> do
      let ast = minifyJS a
      print (getTypes ast)
      writeFile "./samples/1.js" (file <> extraShit)
      print ast
