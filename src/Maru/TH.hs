-- |
-- Avoid 'GHC stage restriction' of TemplateHaskell.
-- Define 'DecsQ' functions.
module Maru.TH
  ( makeLensesA
  ) where

import Control.Lens ((&), (.~), DefName(..), makeLensesWith, lensRules, lensField)
import Language.Haskell.TH (Name, mkName, nameBase, DecsQ)

-- |
-- makeLenses with 'A' suffix.
-- e.g. replEnv -> replEnvA
makeLensesA :: Name -> DecsQ
makeLensesA = makeLensesWith (lensRules & lensField .~ addSuffix)
  where
    addSuffix :: Name -> [Name] -> Name -> [DefName]
    addSuffix _ _ recordName = [TopName . mkName $ nameBase recordName ++ "A"]
