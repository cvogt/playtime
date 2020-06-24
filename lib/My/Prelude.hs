module My.Prelude
  ( module Control.Applicative,
    module Control.Monad,
    module Data.Bifunctor,
    module Data.Bool,
    module Data.Char,
    module Data.Either,
    module Data.Eq,
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
    module Data.Set,
    module Data.Time.Clock.System,
    module Data.Traversable,
    module Data.Tuple,
    module Foreign,
    module GHC.Float,
    module GHC.Generics,
    module GHC.Integer,
    module GHC.Num,
    module GHC.Show,
    module My.Prelude,
    module Safe,
  )
where

import Control.Applicative ((<*>), pure)
import Control.Monad ((=<<), (>>=), fail, forever, unless, void, when)
import Data.Bifunctor (Bifunctor, bimap, first, second)
import Data.Bool ((&&), Bool (False, True), not, otherwise, (||))
import Data.Char (Char)
import Data.Either (Either (Left, Right), either)
import Data.Eq (Eq ((==)))
import Data.Foldable (Foldable, elem, find, fold, foldl, forM_, for_, length, null, sum, toList)
import Data.Function (($), (.), flip)
import Data.Functor ((<$>), (<&>), fmap)
import Data.Int (Int)
import Data.List (drop, foldl1, foldr1, reverse, take)
import Data.List.NonEmpty (NonEmpty ((:|)), groupWith)
import Data.Map (Map)
import Data.Maybe (Maybe (Just, Nothing), catMaybes, fromMaybe, isJust, isNothing, maybe)
import Data.Monoid ((<>), mempty)
import Data.Ord (Ord ((<), (<=), (>), (>=)))
import Data.Set (Set)
import Data.Time.Clock.System (SystemTime)
import Data.Traversable (for, forM, sequence)
import Data.Tuple (fst, snd, uncurry)
import Foreign (ForeignPtr)
import GHC.Float (Double, Float)
import GHC.Generics (Generic)
import GHC.Integer (Integer)
import GHC.Num ((*), (+), (-), subtract)
import GHC.Show (Show (show))
import Safe (headMay, lastMay)

both :: Data.Bifunctor.Bifunctor p => (a -> d) -> p a a -> p d d
both f = bimap f f
