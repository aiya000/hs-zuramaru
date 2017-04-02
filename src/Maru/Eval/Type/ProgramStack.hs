-- | For `ProgramStack` manupilation
module Maru.Eval.Type.ProgramStack where

import Control.Applicative ((<|>))
import Data.Map.Lazy (Map)
import Maru.Eval.Type
import Prelude hiding (lookup)
import qualified Data.Map.Lazy as M

-- | A MaruIdent and value pair for a stack layer
type NameLayer = Map MaruIdent MaruTerm

-- |
-- A layer.
-- Be piled up one when a program bracket is opened.
-- The first layer is used by standard variables, macros and functions
-- (Ex: print, defun, car, cdr)
type ProgramStack = [NameLayer]


-- | O(nm)
lookup :: MaruIdent -> ProgramStack -> Maybe MaruTerm
lookup _ [] = Nothing
lookup name (layer:stack) = M.lookup name layer <|> lookup name stack
