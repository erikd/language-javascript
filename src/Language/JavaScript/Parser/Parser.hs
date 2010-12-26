module Language.JavaScript.Parser.Parser (
   -- * Parsing 
     parse
   , readJs
   , parseFile  
   -- * Parsing expressions
   -- parseExpr
   , parseUsing
   ) where

import Language.JavaScript.Parser.ParseError
import Language.JavaScript.Parser.Grammar
import Language.JavaScript.Parser.Lexer
import Language.JavaScript.Parser.ParserMonad
import Language.JavaScript.Parser.SrcLocation
import qualified Language.JavaScript.Parser.AST as AST

-- | Parse one compound statement, or a sequence of simple statements. 
-- Generally used for interactive input, such as from the command line of an interpreter. 
-- Return comments in addition to the parsed statements.
parseStmtKeepComments :: String -- ^ The input stream (Javascript source code). 
      -> String -- ^ The name of the Javascript source (filename or input device). 
      -> Either ParseError (AST.JSNode, [Token]) 
         -- ^ An error or maybe the abstract syntax tree (AST) of zero or more Javascript statements, plus comments.
parseStmtKeepComments input srcName = 
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
parse input srcName = 
   execParser parseProgram state 
   where
     state = initialState input 

readJs :: String -> AST.JSNode
readJs input = do
  case (parse input "src") of
    Left msg -> error (show msg)
    Right p -> p  

parseFile :: FilePath -> IO AST.JSNode
parseFile filename =
  do 
     x <- readFile (filename)
     return $ readJs x


-- | Parse one compound statement, or a sequence of simple statements. 
-- Generally used for interactive input, such as from the command line of an interpreter. 
-- Return comments in addition to the parsed statements.
parseUsing :: 
      P AST.JSNode
      -> String -- ^ The input stream (Javascript source code). 
      -> String -- ^ The name of the Javascript source (filename or input device). 
      -> Either ParseError AST.JSNode 
         -- ^ An error or maybe the abstract syntax tree (AST) of zero or more Javascript statements, plus comments.
parseUsing p input srcName = 
   execParser p state 
   where
     state = initialState input

