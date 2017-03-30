-- | For `ProgramStack` manupilation
module Elin.Eval.Type.ProgramStack where

import Control.Applicative ((<|>))
import Data.Map.Lazy (Map)
import Elin.Eval.Type
import Prelude hiding (lookup)
import qualified Data.Map.Lazy as M

-- | A EliIdent and value pair for a stack layer
type NameLayer = Map EliIdent EliTerm

-- |
-- A layer.
-- Be piled up one when a program bracket is opened.
-- The first layer is used by standard variables, macros and functions
-- (Ex: print, defun, car, cdr)
type ProgramStack = [NameLayer]


-- | O(nm)
lookup :: EliIdent -> ProgramStack -> Maybe EliTerm
lookup _ [] = Nothing
lookup name (layer:stack) = M.lookup name layer <|> lookup name stack
