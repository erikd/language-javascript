module Language.JavaScript.Pretty.Printer (
  -- * Printing
  ) where

import Language.JavaScript.Parser
import qualified Blaze.ByteString.Builder as BB

writeJs :: JSNode -> BB.Builder
writeJS (NS node _) = rn node

rn :: Node -> BB.Builder
rn = undefined