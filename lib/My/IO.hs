module My.IO
  ( module Control.Concurrent,
    module Control.Concurrent.MVar,
    module System.IO,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import System.IO (FilePath, IO, putStrLn)
