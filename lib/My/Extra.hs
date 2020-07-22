module My.Extra
  ( module Control.Monad.Extra,
    module Control.Monad.Loops,
    module Data.Tuple.Extra,
    module System.Random,
  )
where

import "monad-extras" Control.Monad.Extra (unfoldM_)
import "extra" Control.Monad.Extra (whenM, whileM)
import Control.Monad.Loops (iterateM_)
import Data.Tuple.Extra (dupe)
import System.Random (StdGen, next) -- FIXME: replace next with uniform once stack upgraded random
