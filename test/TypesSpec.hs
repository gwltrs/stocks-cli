module TypesSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Maybe (fromJust)

import Generators
import Types
import Unsafe

typesTests :: SpecWith ()
typesTests = do
    describe "Types.yyyymmdd" $ do
        it "should create YYYYMMDD when given valid string" $ do
            property
                $ forAll goodYYYYMMDDStr 
                $ \s -> ((ymd s) <&> str) == Just s
        it "should fail when given invalid string" $ do
            property
               $ forAll badYYYYMMDDStr 
               $ \s -> ((ymd s) <&> str) == Nothing
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
    describe "Types.day" $ do
        it "should create day when given valid inputs" $ do
            property
                $ forAll goodDayRaw
                $ \dr -> (day dr <&> raw) == Just dr
        it "should fail when given invalid inputs" $ do
            day (unsafeDayRaw "12340408" 2.5 2.0 0.5 1.0 50) -- open > high
                `shouldBe` Nothing
            day (unsafeDayRaw "11111111" 111.1 100.5 99.9 88.88 777777) -- close < low
                `shouldBe` Nothing
            day (unsafeDayRaw "20201225" 0.0 2.0 1.0 3.0 5) -- open < low, close > high
                `shouldBe` Nothing