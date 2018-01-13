-- | Export short names of quasi-quotes in 'Maru.QQ', for easy to use
module Maru.QQ.ShortName where

import Language.Haskell.TH.Quote (QuasiQuoter)
import qualified Maru.QQ as O

-- | parse
p :: QuasiQuoter
p  = O.parse

-- | parsePreprocess
pp :: QuasiQuoter
pp = O.parsePreprocess

-- | zurae
z :: QuasiQuoter
z = O.zurae
