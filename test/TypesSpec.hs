module TypesSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Functor ((<&>))
import Data.Function ((&))

import Generators (goodYYYYMMDDStr, badYYYYMMDDStr, nonRealFloat)
import Types (
    YYYYMMDD, ymd, ymdStr, 
    NonNegativeInt, nonNegativeInt, int,
    NonNegativeRealFloat, nonNegativeRealFloat, flt)

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
    describe "Types.nonNegativeRealFloat" $ do
        it "should create NonNegativeRealFloat when given valid float" $ do
            property
                $ forAll (choose (0, maxFloat))
                $ \f -> (f & nonNegativeRealFloat <&> flt) == Just f
        it "should fail when given invalid float" $ do
            property
                $ forAll (oneof [choose (minFloat, -0.000000001), nonRealFloat]) 
                $ \f -> (f & nonNegativeRealFloat <&> flt) == Nothing

-- Approximate minimum value of IEEE single-precision float
minFloat :: Float
minFloat = -3.4 * 10 ** 38

-- Approximate maximum value of IEEE single-precision float
maxFloat :: Float
maxFloat = 3.4 * 10 ** 38