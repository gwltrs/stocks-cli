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
    describe "Types.day" $ do
        it "should create day when given valid inputs" $ do
            property
                $ forAll goodDayRaw
                $ \dr -> (day dr <&> raw) == Just dr
        it "should fail when given invalid inputs" $ do
            day (unsafeDayRaw "12340408" 250 200 050 100 50) -- open > high
                `shouldBe` Nothing
            day (unsafeDayRaw "11111111" 111 100 99 88 777777) -- close < low
                `shouldBe` Nothing
            day (unsafeDayRaw "20201225" 0 200 100 300 5) -- open < low, close > high
                `shouldBe` Nothing
    describe "Types.stock" $ do
        it "should create stock when given valid inputs" $ do
            property
               $ forAll goodStockArgs
               $ \args -> 
                   (args & (uncurry stock) <&> (\m -> (symbol m, days m))) == Just args
        it "should fail when given invalid inputs" $ do
            property
                $ forAll badStockArgs
                $ \args -> 
                    (args & (uncurry stock) <&> (\m -> (symbol m, days m))) == Nothing
    describe "Types.centsFromDollars" $ do
        it "should convert dollars double to cents integer" $ do
            centsFromDollars 0.0 `shouldBe` 0
            centsFromDollars (-0.003) `shouldBe` 0
            centsFromDollars 0.1 `shouldBe` 10
            centsFromDollars 0.02 `shouldBe` 2
            centsFromDollars 3.45 `shouldBe` 345
            centsFromDollars (-12.34) `shouldBe` -1234
            centsFromDollars 10.109 `shouldBe` 1011
            centsFromDollars 0.9999 `shouldBe` 100
            centsFromDollars 0.400001 `shouldBe` 40