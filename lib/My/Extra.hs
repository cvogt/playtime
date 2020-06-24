module My.Extra
  ( module Control.Monad.Extra,
    module Control.Monad.Loops,
  )
where

import "monad-extras" Control.Monad.Extra (unfoldM_)
import "extra" Control.Monad.Extra (whenM)
import Control.Monad.Loops (iterateM_)
