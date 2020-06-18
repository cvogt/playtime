module My.Prelude
  ( module Control.Applicative,
    module Control.Monad,
    module Data.Bool,
    module Data.Char,
    module Data.Either,
    module Data.Eq,
    module Data.Function,
    module Data.Functor,
    module Data.Int,
    module Data.Maybe,
    module Data.Monoid,
    module Data.Tuple,
    module GHC.Float,
    module GHC.Num,
    module GHC.Show,
    module Safe,
  )
where

import Control.Applicative (pure)
import Control.Monad ((=<<), (>>=))
import Control.Monad (fail)
import Data.Bool (Bool (False, True))
import Data.Char (Char)
import Data.Either (either)
import Data.Eq (Eq ((==)))
import Data.Function (($), (.))
import Data.Functor ((<$>), (<&>), fmap)
import Data.Int (Int)
import Data.Maybe (Maybe (Just, Nothing), isJust, isNothing)
import Data.Monoid ((<>))
import Data.Tuple (uncurry)
import GHC.Float (Double, Float)
import GHC.Num ((*), (+), (-))
import GHC.Show (Show (show))
import Safe (headMay, lastMay)
