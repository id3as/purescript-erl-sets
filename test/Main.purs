module Test.Main where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Data.Map as M
import Erl.Data.Set (Set)
import Erl.Data.Set as S
import Erl.Test.EUnit (runTests, suite, test)
import Test.Assert (assert, assertEqual)

main :: Effect Unit
main =
  void $ runTests do
    suite "Test Erl.Data.Set" do
      test "An empty Set is empty" do
        assert $ S.isEmpty S.empty

      test "An singleton Set contains its element and is of size 1" do
        let s = S.singleton 42
        assert $ S.member 42 s
        assertEqual { actual: S.size s
                    , expected: 1
                    }

      test "fromFoldable - empty" do
        assert $ S.fromFoldable [] == (S.empty :: Set Unit)

      test "fromFoldable - non empty" do
        let set = S.fromFoldable [0, 1, 1, 2]
        assert $ S.size set == 3
        assert $ S.member 0 set
        assert $ S.member 1 set
        assert $ S.member 2 set

      test "intersection" do
        let
          s1 = S.fromFoldable [1,2,3,4,5]
          s2 = S.fromFoldable [2,4,6,8,10]
          s3 = S.fromFoldable  [2,4]
        assert $ S.intersection s1 s2 == s3

      test "catMaybes - drops Nothing values" do
        let s1 = S.fromFoldable [Just 1,Just 2,Just 3,Nothing]
            s2 = S.fromFoldable [1,2,3]
        assert $ S.catMaybes s1 == s2

      test "toggle - inserts item" do
        assert $ S.toggle 1 S.empty == S.fromFoldable [1]

      test "toggle - deletes item" do
        assert $ S.toggle 1 (S.fromFoldable [1]) == S.empty

      test "insert - adds a new element" do
        let
          s = S.fromFoldable [1, 2, 3]
          s' = S.insert 4 s
        assert $ S.member 4 s'
        assertEqual { actual: S.size s', expected: 4 }

      test "insert - does not change the set if element is already present" do
        let
          s = S.fromFoldable [1, 2, 3]
          s' = S.insert 2 s
        assertEqual { actual: S.size s', expected: 3 }
        assert $ s' == s

      test "delete - removes an element" do
        let
          s = S.fromFoldable [1, 2, 3]
          s' = S.delete 2 s
        assert $ not (S.member 2 s')
        assertEqual { actual: S.size s', expected: 2 }

      test "delete - does not change the set if element is not present" do
        let
          s = S.fromFoldable [1, 2, 3]
          s' = S.delete 4 s
        assertEqual { actual: S.size s', expected: 3 }
        assert $ s' == s

      test "union - combines two sets" do
        let
          s1 = S.fromFoldable [1, 2, 3]
          s2 = S.fromFoldable [3, 4, 5]
          s3 = S.union s1 s2
          expected = S.fromFoldable [1, 2, 3, 4, 5]
        assert $ s3 == expected

      test "union - with an empty set" do
        let
          s1 = S.fromFoldable [1, 2, 3]
          s2 = S.empty
          s3 = S.union s1 s2
        assert $ s3 == s1

      test "unions - combines a list of sets" do
        let
          s1 = S.fromFoldable [1, 2]
          s2 = S.fromFoldable [3, 4]
          s3 = S.fromFoldable [1, 5]
          s4 = S.unions [s1, s2, s3]
          expected = S.fromFoldable [1, 2, 3, 4, 5]
        assert $ s4 == expected

      test "difference - returns the difference of two sets" do
        let
          s1 = S.fromFoldable [1, 2, 3, 4, 5]
          s2 = S.fromFoldable [2, 4, 6]
          s3 = S.difference s1 s2
          expected = S.fromFoldable [1, 3, 5]
        assert $ s3 == expected

      test "subset - returns true if the first set is a subset of the second" do
        let
          s1 = S.fromFoldable [1, 2]
          s2 = S.fromFoldable [1, 2, 3]
        assert $ S.subset s1 s2

      test "subset - returns false if the first set is not a subset of the second" do
        let
          s1 = S.fromFoldable [1, 2, 4]
          s2 = S.fromFoldable [1, 2, 3]
        assert $ not (S.subset s1 s2)

      test "properSubset - returns true if the first set is a proper subset of the second" do
        let
          s1 = S.fromFoldable [1, 2]
          s2 = S.fromFoldable [1, 2, 3]
        assert $ S.properSubset s1 s2

      test "properSubset - returns false if sets are equal" do
        let
          s1 = S.fromFoldable [1, 2, 3]
          s2 = S.fromFoldable [1, 2, 3]
        assert $ not (S.properSubset s1 s2)

      test "properSubset - returns false if the first set is not a subset of the second" do
        let
          s1 = S.fromFoldable [1, 2, 4]
          s2 = S.fromFoldable [1, 2, 3]
        assert $ not (S.properSubset s1 s2)

      test "map - applies a function to each element" do
        let
          s1 = S.fromFoldable [1, 2, 3]
          s2 = S.map (_ * 2) s1
          expected = S.fromFoldable [2, 4, 6]
        assert $ s2 == expected

      test "map - can result in a smaller set" do
        let
          s1 = S.fromFoldable [1, 2, 3, 4]
          s2 = S.map (const 1) s1
          expected = S.singleton 1
        assert $ s2 == expected

      test "filter - filters a set" do
        let
          s1 = S.fromFoldable [1, 2, 3, 4, 5]
          s2 = S.filter (_ > 3) s1
          expected = S.fromFoldable [4, 5]
        assert $ s2 == expected

      test "mapMaybe - maps and filters a set" do
        let
          s1 = S.fromFoldable [1, 2, 3, 4, 5]
          s2 = S.mapMaybe (\x -> if x > 3 then Just (x * 2) else Nothing) s1
          expected = S.fromFoldable [8, 10]
        assert $ s2 == expected

      test "toUnfoldable - unfolds a set into an array" do
        let
          s = S.fromFoldable [3, 1, 2]
          a = S.toUnfoldable s :: Array Int
        assertEqual { actual: A.sort a, expected: [1, 2, 3] }

      test "findMin - finds the minimum element in a set" do
        let
          s = S.fromFoldable [3, 1, 2]
        assertEqual { actual: S.findMin s, expected: Just 1 }

      test "findMin - returns Nothing for an empty set" do
        let
          s = S.empty :: Set Int
        assertEqual { actual: S.findMin s, expected: Nothing }

      test "findMax - finds the maximum element in a set" do
        let
          s = S.fromFoldable [3, 1, 2]
        assertEqual { actual: S.findMax s, expected: Just 3 }

      test "findMax - returns Nothing for an empty set" do
        let
          s = S.empty :: Set Int
        assertEqual { actual: S.findMax s, expected: Nothing }

      test "toMap - converts a set to a map" do
        let
          s = S.fromFoldable [1, 2, 3]
          m = S.toMap s
          expected = M.fromFoldable [Tuple 1 unit, Tuple 2 unit, Tuple 3 unit]
        assertEqual { actual: m, expected: expected }

      test "fromMap - converts a map to a set" do
        let
          m = M.fromFoldable [Tuple 1 unit, Tuple 2 unit, Tuple 3 unit]
          s = S.fromMap m
          expected = S.fromFoldable [1, 2, 3]
        assertEqual { actual: s, expected: expected }










