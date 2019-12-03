-- | Uses [classy-prelude](https://hackage.haskell.org/package/classy-prelude) as default Prelude.

module Prelude
    ( module ClassyPrelude
    )
where

import           ClassyPrelude           hiding ( catchIO )
