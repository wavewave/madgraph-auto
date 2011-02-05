module SimpleQQ (quote) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

-- quote :: QuasiQuoter
quote = QuasiQuoter (litE . stringL) (litP . stringL)
