module My.Prelude
  ( module Control.Applicative,
    module Control.Monad,
    module Data.Bool,
    module Data.Char,
    module Data.Either,
    module Data.Eq,
    module Data.Foldable,
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
import Control.Monad (unless, when)
import Data.Bool (Bool (False, True), not)
import Data.Char (Char)
import Data.Either (either)
import Data.Eq (Eq ((==)))
import Data.Foldable (fold, foldl)
import Data.Function (($), (.), flip)
import Data.Functor ((<$>), (<&>), fmap)
import Data.Int (Int)
import Data.Maybe (Maybe (Just, Nothing), catMaybes, isJust, isNothing)
import Data.Monoid ((<>))
import Data.Tuple (uncurry)
import GHC.Float (Double, Float)
import GHC.Num ((*), (+), (-))
import GHC.Show (Show (show))
import Safe (headMay, lastMay)
