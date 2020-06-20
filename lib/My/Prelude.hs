module My.Prelude
  ( module Control.Applicative,
    module Control.Arrow,
    module Control.Monad,
    module Data.Bool,
    module Data.Char,
    module Data.Either,
    module Data.Eq,
    module Data.Foldable,
    module Data.Function,
    module Data.Functor,
    module Data.Int,
    module Data.List,
    module Data.Map,
    module Data.Maybe,
    module Data.Monoid,
    module Data.Ord,
    module Data.Set,
    module Data.Tuple,
    module Foreign,
    module GHC.Float,
    module GHC.Num,
    module GHC.Show,
    module Safe,
  )
where

import Control.Applicative (pure)
import Control.Arrow (first, second)
import Control.Monad ((=<<), (>>=), fail, forever, unless, void, when)
import Data.Bool ((&&), Bool (False, True), not, otherwise, (||))
import Data.Char (Char)
import Data.Either (either)
import Data.Eq (Eq ((==)))
import Data.Foldable (elem, find, fold, foldl, length, null, sum, toList)
import Data.Function (($), (.), flip)
import Data.Functor ((<$>), (<&>), fmap)
import Data.Int (Int)
import Data.List (drop, take)
import Data.Map (Map)
import Data.Maybe (Maybe (Just, Nothing), catMaybes, isJust, isNothing, maybe)
import Data.Monoid ((<>), mempty)
import Data.Ord (Ord ((<), (<=), (>), (>=)))
import Data.Set (Set)
import Data.Tuple (fst, snd, uncurry)
import Foreign (ForeignPtr)
import GHC.Float (Double, Float)
import GHC.Num ((*), (+), (-), subtract)
import GHC.Show (Show (show))
import Safe (headMay, lastMay)
