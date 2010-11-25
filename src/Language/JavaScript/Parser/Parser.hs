module Language.JavaScript.Parser.Parser (
   -- * Parsing modules
   -- parseModule,
   -- * Parsing statements
   parseStmt
   -- * Parsing expressions
   -- parseExpr) 
   , parseUsing
   ) where

import Language.JavaScript.Parser.ParseError
import Language.JavaScript.Parser.Grammar
import Language.JavaScript.Parser.Lexer
import Language.JavaScript.Parser.ParserMonad 
import Language.JavaScript.Parser.SrcLocation
import Language.JavaScript.Parser.Token
import qualified Language.JavaScript.Parser.AST as AST

-- | Parse one compound statement, or a sequence of simple statements. 
-- Generally used for interactive input, such as from the command line of an interpreter. 
-- Return comments in addition to the parsed statements.
parseStmtKeepComments :: String -- ^ The input stream (python statement source code). 
      -> String -- ^ The name of the python source (filename or input device). 
      -> Either ParseError (AST.JSNode, [Token]) 
         -- ^ An error or maybe the abstract syntax tree (AST) of zero or more Javascript statements, plus comments.
parseStmtKeepComments input srcName = 
   execParserKeepComments parse state 
   where
     initLoc = initialSrcLocation srcName
     state = initialState initLoc input initStartCodeStack


-- | Parse one compound statement, or a sequence of simple statements. 
-- Generally used for interactive input, such as from the command line of an interpreter. 
-- Return comments in addition to the parsed statements.
parseStmt :: String -- ^ The input stream (python statement source code). 
      -> String -- ^ The name of the python source (filename or input device). 
      -> Either ParseError AST.JSNode 
         -- ^ An error or maybe the abstract syntax tree (AST) of zero or more Javascript statements, plus comments.
parseStmt input srcName = 
   execParser parse state 
   where
     initLoc = initialSrcLocation srcName
     state = initialState initLoc input initStartCodeStack


-- | Parse one compound statement, or a sequence of simple statements. 
-- Generally used for interactive input, such as from the command line of an interpreter. 
-- Return comments in addition to the parsed statements.
-- parseUsing :: 
--       P Token
--       ->  String -- ^ The input stream (python statement source code). 
--       -> String -- ^ The name of the python source (filename or input device). 
--       -> Either ParseError AST.JSNode 
         -- ^ An error or maybe the abstract syntax tree (AST) of zero or more Javascript statements, plus comments.
parseUsing p input srcName = 
   execParser p state 
   where
     initLoc = initialSrcLocation srcName
     state = initialState initLoc input initStartCodeStack
