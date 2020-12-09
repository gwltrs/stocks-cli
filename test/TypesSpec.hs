module TypesSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Maybe (fromJust)

import Generators
import Types

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
    describe "Types.day" $ do
        it "should create day when given valid inputs" $ do
            property
                $ forAll goodDayProperties
                $ \gdp -> ((uncurry6 day) gdp <&> dayToTuple) == Just gdp
        it "should fail when given invalid inputs" $ do
            dayFromLiterals "12340408" 2.5 2.0 0.5 1.0 50 -- open > high
                `shouldBe` Nothing
            dayFromLiterals "11111111" 111.1 100.5 99.9 88.88 777777 -- close < low
                `shouldBe` Nothing
            dayFromLiterals "20201225" 0.0 2.0 1.0 3.0 5 -- open < low, close > high
                `shouldBe` Nothing

uncurry6 :: (a -> b -> c -> d -> e -> f -> g) -> (a, b, c, d, e, f) -> g
uncurry6 f (x, y, z, w, v, u) = f x y z w v u

dayToTuple :: Day -> (
    YYYYMMDD, 
    NonNegativeRealFloat, 
    NonNegativeRealFloat, 
    NonNegativeRealFloat, 
    NonNegativeRealFloat, 
    NonNegativeInt)
dayToTuple d = (date d, open d, high d, low d, close d, volume d)

dayFromLiterals :: String -> Float -> Float -> Float -> Float -> Int -> Maybe Day
dayFromLiterals d o h l c v = 
    day 
        (fromJust $ ymd $ d)
        (fromJust $ nonNegativeRealFloat $ o)
        (fromJust $ nonNegativeRealFloat $ h)
        (fromJust $ nonNegativeRealFloat $ l)
        (fromJust $ nonNegativeRealFloat $ c)
        (fromJust $ nonNegativeInt $ v)