module Language.JavaScript.Parser
  ( PA.parse,
    PA.parseModule,
    PA.readJs,
    PA.readJsModule,
    PA.parseFile,
    PA.parseFileUtf8,
    PA.showStripped,
    PA.showStrippedMaybe,

    -- * AST elements
    JSExpression (..),
    JSAnnot (..),
    JSBinOp (..),
    JSBlock (..),
    JSUnaryOp (..),
    JSSemi (..),
    JSAssignOp (..),
    JSTryCatch (..),
    JSStatement (..),
    JSSwitchParts (..),
    JSAST (..),
    CommentAnnotation (..),
    -- , ParseError(..)
    -- Source locations
    TokenPosn (..),
    tokenPosnEmpty,

    -- * Pretty Printing
    renderJS,
    renderToString,
    renderToText,
  )
where

import Language.JavaScript.Parser.AST
import qualified Language.JavaScript.Parser.Parser as PA
import Language.JavaScript.Parser.SrcLocation
import Language.JavaScript.Parser.Token
import Language.JavaScript.Pretty.Printer

-- EOF
