module Language.JavaScript.Parser.Parser (
   -- * Parsing
     parse
   , readJs
   , readJsKeepComments
   , parseFile
   -- * Parsing expressions
   -- parseExpr
   , parseUsing
   , showStripped
   , showStrippedMaybe
   ) where

import Language.JavaScript.Parser.ParseError
--import Language.JavaScript.Parser.Grammar
import Language.JavaScript.Parser.Grammar5
import Language.JavaScript.Parser.Lexer
import Language.JavaScript.Parser.ParserMonad
import qualified Language.JavaScript.Parser.AST as AST

-- | Parse one compound statement, or a sequence of simple statements.
-- Generally used for interactive input, such as from the command line of an interpreter.
-- Return comments in addition to the parsed statements.
parseStmtKeepComments :: String -- ^ The input stream (Javascript source code).
      -> String -- ^ The name of the Javascript source (filename or input device).
      -> Either ParseError (AST.JSNode, [Token])
         -- ^ An error or maybe the abstract syntax tree (AST) of zero or more Javascript statements, plus comments.
parseStmtKeepComments input _srcName =
   execParserKeepComments parseProgram state
   where
     state = initialState input


-- | Parse one compound statement, or a sequence of simple statements.
-- Generally used for interactive input, such as from the command line of an interpreter.
-- Return comments in addition to the parsed statements.
parse :: String -- ^ The input stream (Javascript source code).
      -> String -- ^ The name of the Javascript source (filename or input device).
      -> Either ParseError AST.JSNode
         -- ^ An error or maybe the abstract syntax tree (AST) of zero or more Javascript statements, plus comments.
parse input _srcName =
   execParser parseProgram state
   where
     state = initialState input

readJs :: String -> AST.JSNode
readJs input = do
  case (parse input "src") of
    Left msg -> error (show msg)
    Right p -> p

readJsKeepComments :: String -> (AST.JSNode, [Token])
readJsKeepComments input = do
  case (parseStmtKeepComments input "src") of
    Left msg -> error (show msg)
    Right p -> p

parseFile :: FilePath -> IO AST.JSNode
parseFile filename =
  do
     x <- readFile (filename)
     return $ readJs x

showStripped ast = AST.showStripped ast

showStrippedMaybe maybeAst = do
  case maybeAst of
    Left msg -> "Left (" ++ show msg ++ ")"
    Right p -> "Right (" ++ AST.showStripped p ++ ")"

-- | Parse one compound statement, or a sequence of simple statements.
-- Generally used for interactive input, such as from the command line of an interpreter.
-- Return comments in addition to the parsed statements.
parseUsing ::
      P AST.JSNode
      -> String -- ^ The input stream (Javascript source code).
      -> String -- ^ The name of the Javascript source (filename or input device).
      -> Either ParseError AST.JSNode
         -- ^ An error or maybe the abstract syntax tree (AST) of zero or more Javascript statements, plus comments.
parseUsing p input _srcName =
   execParser p state
   where
     state = initialState input

