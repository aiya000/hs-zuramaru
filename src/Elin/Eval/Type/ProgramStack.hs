-- | For `ProgramStack` manupilation
module Elin.Eval.Type.ProgramStack where

import Control.Applicative ((<|>))
import Data.Map.Lazy (Map)
import Elin.Eval.Type
import Prelude hiding (lookup)
import qualified Data.Map.Lazy as M

-- | A EliIdent and value pair for a stack layer
type NameLayer = Map EliIdent EliValue

-- |
-- A layer.
-- Be piled up one when a program bracket is opened.
-- The first layer is used by standard variables, macros and functions
-- (Ex: print, defun, car, cdr)
type ProgramStack = [NameLayer]


-- | Eliningen builtin variables, macros and functions
emptyProgramStack :: ProgramStack
emptyProgramStack = M.fromList [ ("print", error "print")
                               , ("defun", error "defun")
                               , ("defvar", error "defvar")
                               , ("lambda", error "lambda")
                               , ("let", error "let")
                               ]


-- | O(nm)
lookup :: EliIdent -> ProgramStack -> Maybe EliValue
lookup _ [] = Nothing
lookup name (layer:stack) = M.lookup name layer <|> lookup name stack
