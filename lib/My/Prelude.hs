module My.Prelude
  ( module Control.Applicative,
    module Control.DeepSeq,
    module Control.Monad,
    module Control.Monad.Except,
    module Data.Bifunctor,
    module Data.Bool,
    module Data.Char,
    module Data.Data,
    module Data.Either,
    module Data.Eq,
    module Data.Fixed,
    module Data.Foldable,
    module Data.Function,
    module Data.Functor,
    module Data.Int,
    module Data.List,
    module Data.List.NonEmpty,
    module Data.Map,
    module Data.Maybe,
    module Data.Monoid,
    module Data.Ord,
    module Data.Semigroup,
    module Data.Sequence,
    module Data.Set,
    module Data.Text,
    module Data.Time.Clock.System,
    module Data.Traversable,
    module Data.Tuple,
    module Foreign,
    module GHC.Float,
    module GHC.Generics,
    module GHC.Integer,
    module GHC.Num,
    module GHC.Real,
    module GHC.Show,
    module My.Prelude,
    module Safe,
    module Safe.Foldable,
    module Universum,
    module System.Random,
  )
where

import Control.Applicative ((<*>), (<|>), Alternative, Applicative, pure)
import Control.DeepSeq (NFData)
import Control.Monad ((<=<), (=<<), (>>), (>>=), Monad, fail, filterM, foldM, forever, join, mfilter, return, unless, void, when)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.Bifunctor (Bifunctor, bimap, first, second)
import Data.Bool ((&&), Bool (False, True), not, otherwise, (||))
import Data.Char (Char)
import Data.Data (toConstr)
import Data.Either (Either (Left, Right), either)
import Data.Eq (Eq ((/=), (==)))
-- UNSAFE, DO NOT IMPORT: foldl1, foldr1

import Data.Fixed (mod') -- mod' is incorrect for large Doubles and always returns 0.
import Data.Foldable (Foldable, all, any, elem, find, fold, foldl, foldr, forM_, for_, length, mapM_, null, sum, toList, traverse_)
import Data.Function (($), (.), flip, id)
import Data.Functor (($>), (<$), (<$>), (<&>), Functor, fmap)
import Data.Int (Int)
import Data.List (concat, drop, dropWhile, filter, nub, replicate, reverse, sort, take, takeWhile)
import qualified Data.List.NonEmpty
import Data.List.NonEmpty (NonEmpty ((:|)), groupAllWith, groupBy, groupWith, head, iterate, last, repeat, unfoldr)
import Data.Map (Map, keys, mapKeys)
import qualified Data.Map
import Data.Maybe (Maybe (Just, Nothing), catMaybes, fromMaybe, isJust, isNothing, maybe)
import Data.Monoid ((<>), Monoid, mempty)
import Data.Ord (Ord ((<), (<=), (>), (>=)), max, min)
import Data.Semigroup (Semigroup)
import Data.Sequence (iterateN)
import Data.Set (Set, difference, map, union)
import qualified Data.Set
import Data.Text (Text)
import Data.Time.Clock.System (SystemTime)
import Data.Traversable (for, forM, sequence, traverse)
import Data.Tuple (fst, snd, uncurry)
import Foreign (ForeignPtr)
import GHC.Float ((**), Double, Float, divideDouble)
import GHC.Generics (Generic)
import GHC.Integer (Integer)
import GHC.Num ((*), (+), (-), Num, abs, subtract)
import GHC.Real (Integral)
import GHC.Show (Show (show))
import Safe (headMay, lastMay)
import Safe.Foldable (foldl1Safe, foldr1Safe)
import System.Random (StdGen, next) -- FIXME: replace next with uniform once stack upgraded random
import Universum (foldl1, foldr1)

mapDelete :: Ord k => k -> Map k a -> Map k a
mapDelete = Data.Map.delete

mapFromList :: Ord k => [(k, a)] -> Map k a
mapFromList = Data.Map.fromList

mapInsert :: Ord k => k -> a -> Map k a -> Map k a
mapInsert = Data.Map.insert

mapLookup :: Ord k => k -> Map k a -> Maybe a
mapLookup = Data.Map.lookup

mapToList :: Map k a -> [(k, a)]
mapToList = Data.Map.toList

mapSingleton :: k -> a -> Map k a
mapSingleton = Data.Map.singleton

setDelete :: Ord a => a -> Set a -> Set a
setDelete = Data.Set.delete

setFilter :: (a -> Bool) -> Set a -> Set a
setFilter = Data.Set.filter

setFromList :: Ord a => [a] -> Set a
setFromList = Data.Set.fromList

setInsert :: Ord a => a -> Set a -> Set a
setInsert = Data.Set.insert

setMember :: Ord a => a -> Set a -> Bool
setMember = Data.Set.member

setSingleton :: a -> Set a
setSingleton = Data.Set.singleton

nelTakeWhile :: (a -> Bool) -> NonEmpty a -> [a]
nelTakeWhile = Data.List.NonEmpty.takeWhile

-- similar to both in lens
both :: Data.Bifunctor.Bifunctor p => (a -> d) -> p a a -> p d d
both f = bimap f f

-- similar to (??) in lens
(??) :: Functor f => f (a -> b) -> a -> f b
(??) ff x = (\f -> f x) <$> ff

-- named version of (??), name inspired by relude
flap :: Functor f => f (a -> b) -> a -> f b
flap = (??)
