module Erl.Data.Set
  ( Set
  , fromFoldable
  , toUnfoldable
  , empty
  , isEmpty
  , singleton
  , map
  , insert
  , member
  , delete
  , toggle
  , size
  , findMin
  , findMax
  , union
  , unions
  , difference
  , subset
  , properSubset
  , intersection
  , filter
  , mapMaybe
  , catMaybes
  , toMap
  , fromMap
  ) where
import Prelude hiding (map)

import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Maybe (Maybe)
import Data.Ord (class Ord1)
import Data.Unfoldable (class Unfoldable)
import Erl.Data.List (List)
import Erl.Data.List as List
import Erl.Data.Map as M

-- | `Set a` represents a set of values of type `a`
foreign import data Set :: Type -> Type

-- | Create a set from a foldable structure.
fromFoldable :: forall f a. Foldable f => f a -> Set a
fromFoldable = foldl (\s a -> insert a s) empty

-- | Convert a set to an unfoldable structure.
toUnfoldable :: forall f a. Unfoldable f => Set a -> f a
toUnfoldable = List.toUnfoldable <<< toList

foreign import eqImpl :: forall a. Set a -> Set a -> Boolean
instance eqSet :: Eq (Set a) where
  eq = eqImpl

instance eq1Set :: Eq1 Set where
  eq1 = eqImpl

instance showSet :: Show a => Show (Set a) where
  show s = "(fromFoldable " <> show (toList s) <> ")"

instance ordSet :: Ord a => Ord (Set a) where
  compare s1 s2 = compare (toList s1) (toList s2)

instance ord1Set :: Ord1 Set where
  compare1 = compare

instance monoidSet :: Monoid (Set a) where
  mempty = empty

instance semigroupSet :: Semigroup (Set a) where
  append = union

foreign import foldImpl :: forall a b. (Fn2 a b b) -> b -> Set a -> b

instance foldableSet :: Foldable Set where
  foldMap f = foldMap f <<< toList
  foldl f x = foldImpl (mkFn2 \b a -> f a b) x
  foldr f x = foldr f x <<< toList

-- | An empty set
foreign import empty :: forall a. Set a

-- | Test if a set is empty
foreign import isEmpty :: forall a. Set a -> Boolean

-- | Create a set with one element
foreign import singleton :: forall a. a -> Set a

-- | Maps over the values in a set.
-- |
-- | This operation is not structure-preserving for sets, so is not a valid
-- | `Functor`. An example case: mapping `const x` over a set with `n > 0`
-- | elements will result in a set with one element.
foreign import map :: forall a b. (a -> b) -> Set a -> Set b

-- | Test if a value is a member of a set
foreign import member :: forall a. a -> Set a -> Boolean

-- | Insert a value into a set
foreign import insert :: forall a. a -> Set a -> Set a

-- | Delete a value from a set
foreign import delete :: forall a. a -> Set a -> Set a

-- | Insert a value into a set if it is not already present, if it is present, delete it.
foreign import toggle :: forall a. a -> Set a -> Set a

-- | Find the size of a set
foreign import size :: forall a. Set a -> Int

foreign import findMin :: forall a. Set a -> Maybe a

foreign import findMax :: forall a. Set a -> Maybe a

-- | Form the union of two sets
foreign import union :: forall a. Set a -> Set a -> Set a

-- | Form the union of a collection of sets
unions :: forall f a. Foldable f => f (Set a) -> Set a
unions = foldl union empty

-- | Form the set difference
foreign import difference :: forall a. Set a -> Set a -> Set a

-- | True if and only if every element in the first set
-- | is an element of the second set
foreign import subset :: forall a. Set a -> Set a -> Boolean

-- | True if and only if the first set is a subset of the second set
-- | and the sets are not equal
foreign import properSubset :: forall a. Set a -> Set a -> Boolean

-- | The set of elements which are in both the first and second set
foreign import intersection :: forall a. Set a -> Set a -> Set a

-- | Filter out those values of a set for which a predicate on the value fails
-- | to hold.
foreign import filter :: forall a. (a -> Boolean) -> Set a -> Set a

-- | Applies a function to each value in a set, discarding entries where the
-- | function returns `Nothing`.
foreign import mapMaybe :: forall a b. (a -> Maybe b) -> Set a -> Set b

-- | Filter a set of optional values, discarding values that contain `Nothing`
catMaybes :: forall a. Set (Maybe a) -> Set a
catMaybes = mapMaybe identity

-- | A set is a map with no value attached to each key.
foreign import toMap :: forall a. Set a -> M.Map a Unit

-- | A map with no value attached to each key is a set.
-- -- | See also `Data.Map.keys`.
foreign import fromMap :: forall a. M.Map a Unit -> Set a

------------------------------------------------
-- Internal functions
------------------------------------------------
foreign import toList :: forall a. Set a -> List a
