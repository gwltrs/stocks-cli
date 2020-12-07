module TypesSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Functor ((<&>))
import Data.Function ((&))

import Generators (goodYYYYMMDDStr, badYYYYMMDDStr)
import Types (YYYYMMDD, ymd, ymdStr, NonNegativeInt, nonNegativeInt, int)

typesTests :: SpecWith ()
typesTests = do
    describe "Types.yyyymmdd" $ do
        it "should create YYYYMMDD when given valid string" $ do
            property
                $ forAll goodYYYYMMDDStr 
                $ \str -> ((ymd str) <&> ymdStr) == Just str
        it "should fail when given invalid string" $ do
            property
               $ forAll badYYYYMMDDStr 
               $ \str -> ((ymd str) <&> ymdStr) == Nothing
    describe "Types.nonNegativeInt" $ do
        it "should create NonNegativeInt when given valid integer" $ do
            property
                $ forAll (choose (0, maxBound))
                $ \i -> (i & nonNegativeInt <&> int) == Just i
        it "should fail when given invalid integer" $ do
            property
                $ forAll (choose (minBound, -1))
                $ \i -> (i & nonNegativeInt <&> int) == Nothing